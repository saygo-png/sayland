-- | Description : Everything that has to do with making and keeping connections.
module Sayland.Connection (module Sayland.Connection) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (flushTQueue, newTQueue, unGetTQueue, writeTQueue)
import Control.Concurrent.STM.TVar
import Control.Exception (finally)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State.Strict (runStateT)
import Data.Binary (Word16)
import Data.Binary.Get
import Data.Bool
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Coerce
import Data.Data (cast)
import Data.Map qualified as Map
import Data.String
import Debug.Trace (traceIO)
import Foreign (Storable (peek, sizeOf), castPtr)
import Foreign.C
import Network.Socket
import Network.Socket.ByteString (recvMsg)
import Sayland.Internal.Core
import Sayland.Internal.Object
import Sayland.Internal.Prelude
import Sayland.Internal.Protocols.Wayland
import Sayland.Internal.Trace
import Sayland.Wire
import System.Console.ANSI (Color (Magenta), ColorIntensity (Vivid))
import System.Directory (doesFileExist)
import System.Environment.Blank (getEnv)
import System.FilePath
import System.Posix (Fd (Fd))

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

-- | Get a list of file descriptors from an ancillary data bytestring.
decodeFds :: BS.ByteString -> IO [Fd]
decodeFds bs = map Fd <$> go bs []
  where
    intSize = sizeOf (0 :: CInt)
    go b acc
      | BS.length b < intSize = pure $ reverse acc
      | otherwise = do
          let (x, rest) = BS.splitAt intSize b
          v <- BS.useAsCString x (peek . castPtr)
          go rest (v : acc)

-- | Handle communication between a server and a client in provided socket, works both on the server and the client.
clientLoop :: (KnownPerspective p) => Socket -> Wayland p ()
clientLoop = clientLoop' ""
  where
    clientLoop' :: (KnownPerspective p) => BS.ByteString -> Socket -> Wayland p ()
    clientLoop' bytes' sock = do
      queue <- (.fdQueue) <$> getClientEnv
      (_, bytes'', cmsgs, _flags) <- liftIO $ recvMsg sock 8 4096 mempty
      newFds <- liftIO $ concat <$> traverse (decodeFds . cmsgData) (filter (\x -> cmsgId x == CmsgIdFds) cmsgs)
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
      where
        isPartial :: BS.ByteString -> Bool
        isPartial s = case runGetOrFail getHeader (BS.fromStrict s) of
          Left (_, _, _) -> True
          Right (rest, _, (_, _, size')) -> fromIntegral (size' - headerSize) > BL.length rest

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
  case runGetOrFail (runStateT (getMessage opcode) fds) (BS.fromStrict msg) of
    Left (_, _, err) -> fail err
    Right (_, _, (message, leftover)) -> do
      void . atomically $ traverse (unGetTQueue env.fdQueue) (reverse leftover)
      colorize <- liftIO getColorize
      liftIO . traceIO . colorize Vivid Magenta $ ("  <- " <>) $ showMessage oid message
      handle message
      handlers <- readIORef env.eventHandlers
      forM_ handlers $ \(EventHandler f) -> forM_ (cast message) $ f oid

-- }}}

-- Socket Finding Utilities {{{

-- | Get an absolute socket path based from @XDG_RUNTIME_DIR@.
getSocketPath :: IO (Maybe String) -> IO (Maybe FilePath)
getSocketPath = liftA2 (liftA2 (</>)) $ getEnv "XDG_RUNTIME_DIR"

-- | Find an already existing socket, if @WAYLAND_DISPLAY@ does not exist.
openSocketName :: IO (Maybe String)
openSocketName = findSocketName doesFileExist

{- | Find a not already existing and valid socket name.
Does NOT care about @WAYLAND_DISPLAY@
-}
availableSocketName :: IO (Maybe String)
availableSocketName = scanRuntimeDir (fmap not . doesFileExist)

{- | Find a socket name by predicate.
Short circuits if 'WAYLAND_DISPLAY' exists, ignoring the predicate.
-}
findSocketName :: (FilePath -> IO Bool) -> IO (Maybe String)
findSocketName isAccepted = getEnv "WAYLAND_DISPLAY" `orElse'` scanRuntimeDir isAccepted
  where
    -- Run the second action only if the first yields Nothing.
    orElse' :: IO (Maybe a) -> IO (Maybe a) -> IO (Maybe a)
    orElse' a b = a >>= maybe b (pure . Just)

-- | Find a socket name by predicate, scanning @XDG_RUNTIME_DIR@.
scanRuntimeDir :: (FilePath -> IO Bool) -> IO (Maybe String)
scanRuntimeDir isAccepted =
  getEnv "XDG_RUNTIME_DIR"
    >>= maybe (pure Nothing) (\dir -> firstMatch (isAccepted . (dir </>)) candidates)
  where
    candidates :: [String] = ["wayland-" <> fromString (show i) | i <- [0 .. 99 :: Int]]

    -- First element satisfying the predicate, stopping on the first match.
    firstMatch :: (a -> IO Bool) -> [a] -> IO (Maybe a)
    firstMatch p =
      foldr (\x rest -> p x >>= \ok -> if ok then pure (Just x) else rest) (pure Nothing)

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
