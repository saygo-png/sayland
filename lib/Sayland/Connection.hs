-- | Description : Everything that has to do with making and keeping connections.
module Sayland.Connection (module Sayland.Connection) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (flushTQueue, newTQueue, unGetTQueue, writeTQueue)
import Control.Concurrent.STM.TVar
import Control.Exception (finally)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Binary (Word16)
import Data.Bool
import Data.ByteString qualified as BS
import Data.Coerce
import Data.Data (cast)
import Data.Map qualified as Map
import Debug.Trace (traceIO)
import Network.Socket
import Sayland.Internal.Core
import Sayland.Internal.Object
import Sayland.Internal.Prelude
import Sayland.Internal.Protocols.Wayland
import Sayland.Internal.Trace
import Sayland.Wire
import System.Console.ANSI (Color (Magenta), ColorIntensity (Vivid))

-- Listeners {{{

-- | Listen for client connections and handle them with `handleIncomingClient`.
listenForClients :: (MonadIO m) => ServerEnvironment -> m ()
listenForClients env = do
  (sock, _) <- liftIO $ accept env.socket
  liftIO $ traceIO "New client connected."
  handleIncomingClient env sock
  listenForClients env

-- | Deal with an incoming client, creating a client environment and updating the server state.
handleIncomingClient :: (MonadIO m) => ServerEnvironment -> Socket -> m ()
handleIncomingClient env socket' = do
  counter <- newIORef 0
  objects <- newIORef $ Map.fromList [(1, SomeObject $ Wl_display $ TObjectID 1)]
  globals <- newIORef mempty
  fdQueue <- atomically newTQueue
  let clientenv =
        ClientEnvironment
          { socket = socket'
          , counter
          , objects
          , eventHandlers = env.eventHandlers
          , globals
          , interfaceTable = env.interfaceTable
          , fdQueue
          }
  serial' <- atomically $ do
    modifyTVar env.clientSerial (+ 1)
    readTVar env.clientSerial
  atomically . modifyTVar env.clients $ Map.insert serial' clientenv
  void
    . liftIO
    . forkIO
    $ runReaderT (serveClient $ clientLoop socket') (ClientServerEnv env clientenv serial')
    `finally` do
      close socket'
      atomically . modifyTVar env.clients $ Map.delete serial'

{- | Run one client's connection. A protocol violation is reported to the client
before the connection is closed.
-}
serveClient :: Wayland Server () -> Wayland Server ()
serveClient loop =
  loop `catchW` \(e :: ProtocolError) -> do
    sendMsg (Wl_display wlDisplayId) (Event_wl_display_error e.object e.code e.message)
    throwIO e

-- | Handle communication between a server and a client in provided socket, works both on the server and the client.
clientLoop :: (KnownPerspective p) => Socket -> Wayland p ()
clientLoop = clientLoop' ""
  where
    clientLoop' :: (KnownPerspective p) => BS.ByteString -> Socket -> Wayland p ()
    clientLoop' bytes' sock = do
      queue <- (.fdQueue) <$> getClientEnv
      (bytes'', newFds) <- liftIO $ recvChunk sock
      atomically $ mapM_ (writeTQueue queue) newFds
      let bytes = bytes' <> bytes''
      bool
        ( case decodeMessage bytes of
            Just (oid, opcode, x, y) -> do
              handleMessage oid opcode x
              clientLoop' y sock
            Nothing -> error "impossible/undefined edge case"
        )
        (clientLoop' bytes sock)
        (isPartial bytes)

{- | Deal with an inbound message. Checks if the `ObjectID` reference is valid.
if it is valid, the work is handed to `dispatchMessage`.
-}
handleMessage :: (KnownPerspective p) => RawObjectID -> Word16 -> BS.ByteString -> Wayland p ()
handleMessage oid opcode msg = do
  env <- getClientEnv
  objects <- readIORef env.objects
  case Map.lookup oid objects of
    Just (SomeObject o) -> withIncoming o $ dispatchWith (applyIncoming o) oid opcode msg
    Nothing -> liftIO $ traceIO $ "invalid object reference with id: " <> show oid

-- | Parse a message for an object and run the given handler on it, then any registered 'EventHandler's.
dispatchWith :: (Message m) => (m -> Wayland p ()) -> RawObjectID -> Word16 -> BS.ByteString -> Wayland p ()
dispatchWith handle oid opcode msg = do
  env <- getClientEnv
  fds <- atomically $ flushTQueue env.fdQueue
  case runWireGet (getMessage opcode) fds msg of
    Left err -> fail err
    Right (message, leftover) -> do
      void . atomically $ traverse (unGetTQueue env.fdQueue) (reverse leftover)
      colorize <- liftIO getColorize
      liftIO . traceIO . colorize Vivid Magenta $ ("  <- " <>) $ showMessage oid message
      handle message
      handlers <- readIORef env.eventHandlers
      forM_ handlers $ \(EventHandler f) -> forM_ (cast message) $ f oid

-- }}}

-- | Create a default client environment.
waylandSetup :: ProtocolTable -> IO (WaylandEnv Client)
waylandSetup protocolTable = do
  let display = SomeObject $ Wl_display wlDisplayId
  getSocketPath openSocketName >>= \case
    Just path -> do
      putStrLn $ "using socket path: " <> show path
      sock <- socket AF_UNIX Stream defaultProtocol
      connect sock $ SockAddrUnix path
      counter <- newIORef $ coerce wlDisplayId
      objects <- newIORef $ Map.fromList [(coerce wlDisplayId, display)]
      globals <- newIORef mempty
      handlers <- newIORef mempty
      let interfaceTable = Map.fromList protocolTable
      fdqueue <- atomically newTQueue
      pure $ ClientEnv $ ClientEnvironment sock counter objects globals interfaceTable handlers fdqueue
    Nothing -> error "couldn't find `$WAYLAND_DISPLAY`, nor any open socket."

-- vim: foldmethod=marker
