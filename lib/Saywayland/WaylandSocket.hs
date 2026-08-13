module Saywayland.WaylandSocket (module Saywayland.WaylandSocket) where

import Data.Bimap qualified as BM
import Data.Binary.Get
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Data (cast)
import Data.Map qualified as Map
import Debug.Trace (traceIO)
import Foreign (Storable (peek, sizeOf), castPtr)
import Foreign.C
import GHC.IO (unsafePerformIO)
import Network.Socket
import Network.Socket.ByteString (recvMsg)
import Relude
import Saywayland.Protocols.Wayland
import Saywayland.Types
import System.Console.ANSI (Color (Magenta), ColorIntensity (Vivid))
import System.Directory (doesFileExist)
import System.Environment.Blank (getEnv)
import System.FilePath
import System.Posix (Fd (Fd))
import Control.Concurrent.STM (writeTQueue, newTQueue, modifyTVar)
import Control.Concurrent.Async (async)

-- Listeners {{{

-- | listen for client connections in provided socket.
listenForClients :: MonadIO m => ServerEnvironment -> m ()
listenForClients env = do
  (sock, _) <- liftIO $ accept env.socket
  liftIO $ traceIO "New client connected."
  handleIncomingClient env sock
  listenForClients env

handleIncomingClient :: MonadIO m => ServerEnvironment -> Socket -> m ()
handleIncomingClient env socket' = do
  counter <- liftIO $ newIORef 0
  objects <- liftIO $ newIORef $ one (1, Interface Wl_display{wlid = 1})
  globals <- liftIO $ newIORef BM.empty
  fdQueue <- liftIO $ atomically newTQueue
  let clientenv = ClientEnvironment
            { socket = socket'
            , counter
            , objects
            , eventHandlers = env.eventHandlers
            , globals
            , interfaceTable = env.interfaceTable
            , versionTable = env.versionTable
            , fdQueue
            }
  serial' <- atomically $ do
    modifyTVar env.clientSerial (+ 1)
    readTVar env.clientSerial
  atomically . modifyTVar env.clients $ Map.insert serial' clientenv
  void . liftIO . async $ runReaderT (clientLoop socket') $ ClientServerEnv env clientenv serial'

getHeader :: Get (Word32, Word16, Word16)
getHeader = (,,) <$> getWord32le <*> getWord16le <*> getWord16le

-- | a monstrocity that gets a list of file descriptors from an ancillary data bytestring
decodeFds :: BS.ByteString -> [Fd]
decodeFds bs =
  Fd <$> go bs []
  where
    go b acc
      | BS.length b < sizeOf (0 :: CInt) = reverse acc
      | otherwise =
          let (x, rest) = BS.splitAt (sizeOf (0 :: CInt)) b
              v =
                unsafePerformIO $
                  BS.useAsCString x (peek . castPtr)
           in go rest (v : acc)

-- | handle communication between a server and a client in provided socket, works both on the server and the client.
clientLoop :: Socket -> Wayland p ()
clientLoop = clientLoop' ""

clientLoop' :: BS.ByteString -> Socket -> Wayland p ()
clientLoop' bytes' sock = do
  queue <- (.fdQueue) <$> getClientEnv
  (_, bytes'', cmsgs, _flags) <- liftIO $ recvMsg sock 8 4096 mempty
  let newFds = concatMap (decodeFds . cmsgData) $ filter (\x -> cmsgId x == CmsgIdFds) cmsgs
  liftIO . atomically $ mapM_ (writeTQueue queue) newFds
  let bytes = bytes' <> bytes''
  bool
    ( case extractMessage bytes of
        Just (oid, opcode, x, y) -> do
          --void $ ask >>= liftIO . async . runReaderT (handleMessage oid opcode x)
          handleMessage oid opcode x
          clientLoop' y sock
        Nothing -> error "impossible/undefined edge case"
    )
    (clientLoop' bytes sock)
    (isPartial bytes)

isPartial :: BS.ByteString -> Bool
isPartial s = case runGetOrFail getHeader (fromStrict s) of
  Left (_, _, _) -> True
  Right (rest, _, (_, _, size')) -> fromIntegral (size' - headerSize) > BL.length rest

extractMessage :: BS.ByteString -> Maybe (Word32, Word16, BS.ByteString, BS.ByteString)
extractMessage s = case runGetOrFail getHeader (fromStrict s) of
  Left (_, _, _) -> Nothing
  Right (rest', _, (oid, opcode, size')) -> Just (oid, opcode, BS.take payload rest, BS.drop payload rest)
    where
      payload = fromIntegral $ size' - headerSize
      rest = BS.toStrict rest'

handleMessage :: ObjectID -> Word16 -> BS.ByteString -> Wayland p ()
handleMessage oid opcode msg = do
  ask >>= \case
    ClientEnv env -> do
      objects <- readIORef env.objects
      case Map.lookup oid objects of
        Just (Interface x) -> dispatchMessage x oid opcode msg
        Nothing -> liftIO $ traceIO $ "invalid object reference with id: " <> show oid
    ClientServerEnv _ env _ -> do
      objects <- readIORef env.objects
      case Map.lookup oid objects of
        Just (Interface x) -> dispatchMessage x oid opcode msg
        Nothing -> liftIO $ traceIO $ "invalid object reference with id: " <> show oid

class Dispatch (p :: Perspective) where
  dispatchMessage :: forall i. (Interface' i p) => i -> ObjectID -> Word16 -> BS.ByteString -> Wayland p ()

instance Dispatch Client where
  dispatchMessage x oid opcode msg = do
    ClientEnv env <- ask
    getter <- liftIO $ getEvent opcode $ AdditionalParserData env.fdQueue
    case runGetOrFail getter (toLazy msg) of
      Left (_, _, err) -> fail err
      Right (_, _, event) -> do
        colorize <- liftIO getColorize
        liftIO . traceIO . colorize Vivid Magenta $ showEvent oid event
        runEvent x event
        handlers <- liftIO $ readIORef env.eventHandlers
        forM_ handlers $ \(EventHandler f) -> for_ (cast event) $ f oid

instance Dispatch Server where
  dispatchMessage x oid opcode msg = do
    ClientServerEnv _ env _ <- ask
    getter <- liftIO $ getEvent opcode $ AdditionalParserData env.fdQueue
    case runGetOrFail getter (toLazy msg) of
      Left (_, _, err) -> fail err
      Right (_, _, event) -> do
        colorize <- liftIO getColorize
        liftIO . traceIO . colorize Vivid Magenta $ showEvent oid event
        runRequest x event
        handlers <- liftIO $ readIORef env.eventHandlers
        forM_ handlers $ \(EventHandler f) -> for_ (cast event) $ f oid

-- }}}

-- Socket Finding Utilities {{{
getXdgRuntimeDir :: IO (Maybe String)
getXdgRuntimeDir = getEnv "XDG_RUNTIME_DIR"

getWaylandDisplay :: IO (Maybe String)
getWaylandDisplay = getEnv "WAYLAND_DISPLAY"

getSocketPath :: IO (Maybe FilePath) -> IO (Maybe FilePath)
getSocketPath = liftA2 (liftA2 (</>)) getXdgRuntimeDir

-- | Find an already existing socket, if the environment variable does not exist
openSocket :: IO (Maybe FilePath)
openSocket = findSocket' doesFileExist

-- | Find a non-existing valid socket file
availableSocket :: IO (Maybe FilePath)
availableSocket = findSocket' (fmap not . doesFileExist)

-- | Helper function allowing filtering of wayland sockets
findSocket' :: (FilePath -> IO Bool) -> IO (Maybe FilePath)
findSocket' f =
  getWaylandDisplay >>= \case
    Just x -> pure $ Just x
    Nothing ->
      getXdgRuntimeDir >>= \case
        Just xdg -> newNumbered (f . (xdg </>)) "wayland-" 0 99
        Nothing -> pure Nothing

-- }}}

-- Helpers {{{
newNumbered :: (FilePath -> IO Bool) -> FilePath -> Int -> Int -> IO (Maybe FilePath)
newNumbered req s i maxi = bool (req this >>= bool (newNumbered req s (i + 1) maxi) (pure $ Just this)) (pure Nothing) (maxi < i)
  where
    this = s <> fromString (show i)

-- }}}

-- vim: foldmethod=marker
