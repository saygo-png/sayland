{-# LANGUAGE RecordWildCards #-}

-- | Description : Everything that has to do with making and keeping connections.
module Sayland.Connection (module Sayland.Connection) where

import Control.Concurrent (forkFinally, killThread, myThreadId, threadDelay, throwTo)
import Control.Exception (AsyncException (ThreadKilled), bracket, displayException, finally, fromException)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Binary (Word16)
import Data.ByteString qualified as BS
import Data.Coerce
import Data.Data (cast)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Network.Socket
import Sayland.Internal.Core
import Sayland.Internal.Prelude
import Sayland.Internal.Protocols.Wayland
import Sayland.Log
import Sayland.Object (sendMsg)
import Sayland.Wire
import System.Posix (removeLink)

-- Listeners {{{

-- | Listen for client connections and handle them with `handleIncomingClient`.
listenForClients :: (MonadIO m) => ServerEnv -> m ()
listenForClients env = do
  (sock, _) <- liftIO $ accept env.listenSocket
  handleIncomingClient env sock
  listenForClients env

-- | Deal with an incoming client, creating its connection environment and updating the server state.
handleIncomingClient :: (MonadIO m) => ServerEnv -> Socket -> m ()
handleIncomingClient env socket' = do
  clientenv <- liftIO $ newConnectionEnv socket' env.interfaceTable env.eventHandlers env.onRecoverable env.serverLogger
  serial' <- atomically $ do
    modifyTVar env.clientSerial (+ 1)
    readTVar env.clientSerial
  atomically . modifyTVar env.clients $ Map.insert serial' clientenv
  let logClient level = logWith env.serverLogger level (Just serial')
  liftIO $ logClient LogDebug "connected"
  void
    . liftIO
    . forkFinally (runReaderT (serveClient $ clientLoop socket') (ClientServerEnv env clientenv serial'))
    $ \ended -> do
      close socket'
      atomically . modifyTVar env.clients $ Map.delete serial'
      case ended of
        Left e | Just Disconnected <- fromException e -> logClient LogDebug "disconnected"
        Left e -> logClient LogError $ "disconnected: " <> displayException e
        Right () -> logClient LogDebug "disconnected"

-- | Run one client's connection. A protocol violation is reported to the client
-- before the connection is closed.
serveClient :: Wayland Server () -> Wayland Server ()
serveClient loop =
  loop `catchW` \(e :: ProtocolError) -> do
    sendMsg (Wl_display wlDisplayId) (Event_wl_display_error (fromMaybe (coerce wlDisplayId) e.object) e.code (fromMaybe mempty e.message))
    throwIO e

-- | Handle communication between a server and a client in provided socket, works both on the server and the client.
clientLoop :: (KnownPerspective p) => Socket -> Wayland p ()
clientLoop = clientLoop' ""
  where
    clientLoop' :: (KnownPerspective p) => BS.ByteString -> Socket -> Wayland p ()
    clientLoop' bytes' sock = do
      queue <- (.fdQueue) <$> getConnectionEnv
      (bytes'', newFds) <- liftIO . peerGone $ recvChunk sock
      -- Nothing to read from a stream socket means the peer closed it.
      when (BS.null bytes'') $ throwIO Disconnected
      atomically $ mapM_ (writeTQueue queue) newFds
      let bytes = bytes' <> bytes''
      case decodeMessage bytes of
        Right (oid, opcode, x, y) -> do
          handleMessage oid opcode x
          clientLoop' y sock
        Left Incomplete -> clientLoop' bytes sock
        Left malformed -> throwIO malformed

-- | Deal with an inbound message. Checks if the `ObjectID` reference is valid.
-- if it is valid, the work is handed to `dispatchWith`.
handleMessage :: (KnownPerspective p) => WlObjectID -> Word16 -> BS.ByteString -> Wayland p ()
handleMessage oid' opcode msg = do
  env <- getConnectionEnv
  objects <- readTVarIO env.objects
  case oid' >>= \oid -> (oid,) <$> Map.lookup oid objects of
    Just (oid, SomeObject o) -> withIncoming o $ dispatchWith (applyIncoming o) oid opcode msg
    Nothing -> logWarn $ "ignored a message to unknown object " <> show oid'

-- | Parse a message for an object and run the given handler on it, then any registered t'EventHandler's.
dispatchWith :: (Message m) => (m -> Wayland p ()) -> AnyObjectID -> Word16 -> BS.ByteString -> Wayland p ()
dispatchWith handle oid opcode msg = do
  env <- getConnectionEnv
  fds <- atomically $ flushTQueue env.fdQueue
  case runGetMessage opcode fds msg of
    -- Malformed for the protocol: reported like a violation the handler found, which a server sends to the client.
    Left err -> throwIO . ProtocolError (Just oid) (errorCode Err_invalid_method) . Just . wlTextLossy . T.pack $ "malformed message with opcode " <> show opcode <> ": " <> show err
    Right (message, leftover) -> do
      void . atomically $ traverse (unGetTQueue env.fdQueue) (reverse leftover)
      logMessage Received oid message
      handle message
      handlers <- readIORef env.eventHandlers
      forM_ handlers $ \(EventHandler f) -> forM_ (cast message) $ f oid

-- }}}

-- Clients {{{

-- | Connect to the compositor (see `waylandSetup`) and run a client on that connection, with its event loop running
-- in the background (see `withEventLoop`). The connection is closed once the client returns or throws.
--
-- > main = withWaylandClient (waylandTable <> xdg_shellTable) $ do
-- >   registry <- getWlDisplay >>= \display -> newObject display Request_wl_display_get_registry
-- >   roundtrip
-- >   compositor <- bindToInterface @Wl_compositor registry
-- >   ...
-- >   untilDisconnected
withWaylandClient :: ProtocolTable -> Wayland Client a -> IO a
withWaylandClient protocolTable client = bracket (waylandSetup protocolTable) waylandClose $ runReaderT (withEventLoop client)

-- | Run a client with the connection's event loop running in another thread, which receives the compositor's
-- events and runs their handlers. The client runs in the calling thread, which matters for bound threads.
--
-- If the event loop ends, the exception it ended with is thrown to the client: t'ConnectionError' `Disconnected`
-- when the compositor hangs up, or a t'ProtocolError' it reported. The event loop is stopped once the client returns or throws.
withEventLoop :: Wayland Client a -> Wayland Client a
withEventLoop client = do
  env <- ask
  conn <- getConnectionEnv
  liftIO $ do
    caller <- myThreadId
    -- The loop only ends with an exception. Hand it to the client, unless the client returning stopped the loop.
    let handOver = \case
          Left e | fromException e /= Just ThreadKilled -> throwTo caller e
          _ -> pass
    loopThread <- forkFinally (runReaderT (clientLoop conn.connectionSocket) env) handOver
    runReaderT client env `finally` killThread loopThread

-- | Block until the compositor hangs up, then return. Other ways the event loop can end are rethrown.
-- Use at the end of a client started with `withWaylandClient` or `withEventLoop` that runs until the compositor goes away.
-- A hangup before this is called is thrown at the client like any other error of the event loop.
untilDisconnected :: Wayland Client ()
untilDisconnected =
  liftIO (forever $ threadDelay 1_000_000_000) `catchW` \case
    Disconnected -> pass
    e -> throwIO e

-- | Close a client's connection to the compositor.
waylandClose :: WaylandEnv Client -> IO ()
waylandClose (ClientEnv conn) = close conn.connectionSocket

-- }}}

-- | Connect to the compositor in @WAYLAND_DISPLAY@, or the first socket found in @XDG_RUNTIME_DIR@.
waylandSetup :: ProtocolTable -> IO (WaylandEnv Client)
waylandSetup protocolTable =
  getSocketPath openSocketName >>= \case
    Just path -> do
      env <- waylandConnect protocolTable path
      runReaderT (logDebug $ "connected to " <> show path) env
      pure env
    Nothing -> throwIO $ NoSocket "couldn't find `$WAYLAND_DISPLAY`, nor any open socket."

-- | Create a server listening on a free socket name in @XDG_RUNTIME_DIR@, like @wayland-0@.
-- Accept its clients with `listenForClients`, and close it with `waylandServerClose`.
waylandServerSetup :: ProtocolTable -> IO ServerEnv
waylandServerSetup protocolTable =
  getSocketPath availableSocketName >>= \case
    Just path -> do
      env <- waylandListen protocolTable path
      logWith env.serverLogger LogDebug Nothing $ "listening on " <> show path
      pure env
    Nothing -> throwIO $ NoSocket "couldn't find `$XDG_RUNTIME_DIR`, nor a free socket name in it."

-- | Create a server listening on a new socket at the given path.
waylandListen :: ProtocolTable -> FilePath -> IO ServerEnv
waylandListen protocolTable socketPath = do
  listenSocket <- socket AF_UNIX Stream defaultProtocol
  bind listenSocket $ SockAddrUnix socketPath
  listen listenSocket 5
  clients <- newTVarIO mempty
  clientSerial <- newTVarIO 0
  eventHandlers <- newIORef mempty
  serverLogger <- defaultLogger Server
  pure ServerEnv{interfaceTable = Map.fromList protocolTable, onRecoverable = Reject, ..}

-- | Close a server's connections to its clients and its socket, and remove the socket's file.
waylandServerClose :: ServerEnv -> IO ()
waylandServerClose env = do
  readTVarIO env.clients >>= mapM_ (close . (.connectionSocket))
  close env.listenSocket
  removeLink env.socketPath

-- | Connect to the compositor socket at the given path.
waylandConnect :: ProtocolTable -> FilePath -> IO (WaylandEnv Client)
waylandConnect protocolTable path = do
  sock <- socket AF_UNIX Stream defaultProtocol
  connect sock $ SockAddrUnix path
  eventHandlers <- newIORef mempty
  ClientEnv <$> (newConnectionEnv sock (Map.fromList protocolTable) eventHandlers Reject =<< defaultLogger Client)

-- | The create the initial state of a new connection.
newConnectionEnv ::
  forall p. (KnownPerspective p) => Socket -> Map.Map WlText InterfaceEntry -> IORef [EventHandler p] -> OnRecoverable -> Logger -> IO (ConnectionEnv p)
newConnectionEnv connectionSocket interfaceTable eventHandlers onRecoverable logger = do
  writeLock <- newMVar ()
  counter <- newMVar (Just firstObjectID)
  nextPeerID <- newIORef $ case toAnyObjectID (firstObjectID @p) of
    FromClient _ -> 0xff000000 -- This is a client, the first ID a server allocates.
    FromServer _ -> 2 -- This is a server, the first ID a client allocates after wl_display.
  objects <- newTVarIO $ Map.singleton (toObjectID wlDisplayId) (SomeObject $ Wl_display wlDisplayId)
  globals <- newIORef mempty
  fdQueue <- atomically newTQueue
  pure ConnectionEnv{..}

-- vim: foldmethod=marker
