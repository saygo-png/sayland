{-# LANGUAGE QuasiQuotes #-}

-- | Description : A compositor and a client from this library, talking over a real socket.
module Integration (tests) where

import Control.Concurrent (ThreadId, forkFinally, forkIO, killThread)
import Control.Concurrent.STM (check)
import Control.Exception (bracket, bracket_)
import Data.ByteString qualified as BS
import Data.Map qualified as Map
import Foreign (castPtr, peekArray)
import GHC.IO.Handle (hFlush)
import Network.Socket
import Relude hiding (hFlush)
import Sayland
import Sayland.Internal.Core (InvalidMessage, ProtocolError (..), TObjectID (..), catchW, errorCode, sendMessage)
import Sayland.Internal.Object (proxyInterface)
import Sayland.Wire
import System.Directory (createDirectory, getTemporaryDirectory, removeDirectoryRecursive)
import System.Posix (ShmOpenFlags (..), fdToHandle, ownerReadMode, ownerWriteMode, setFdSize, shmOpen, shmUnlink, unionFileModes)
import System.Random (randomIO)
import System.Timeout (timeout)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Integration"
    [ testCase "the client binds globals the compositor advertises" bindsGlobals
    , testCase "a file descriptor reaches the compositor with its request" passesFd
    , testCase "a request the client rejects is never sent" rejectsOwnRequest
    , testCase "the compositor reports a protocol violation to the client" reportsViolation
    , testCase "the compositor reports a malformed message to the client" reportsMalformed
    ]

table :: ProtocolTable
table = waylandTable <> xdg_shellTable

-- | The client binds some globals. The objects it then creates exist on the compositor too.
bindsGlobals :: Assertion
bindsGlobals = withSession $ \s -> do
  ClientEnv env <- ask
  advertised <- fmap fst . Map.elems <$> readIORef env.globals
  liftIO $ sort advertised @?= sort (fst <$> table)
  shm <- bindToInterface @Wl_shm s.registry
  wl_compositor <- bindToInterface @Wl_compositor s.registry
  surface <- newObject wl_compositor Request_wl_compositor_create_surface
  roundtrip s
  liftIO $ do
    compositorObject s shm.wlid >>= assertBool "the compositor has no wl_shm" . isJust
    compositorObject s surface.wlid >>= assertBool "the compositor has no wl_surface" . isJust

-- | A pool's fd is sent alongside @create_pool@. The compositor maps the same memory the client writes.
passesFd :: Assertion
passesFd = withSession $ \s -> do
  shm <- bindToInterface @Wl_shm s.registry
  n :: Word32 <- liftIO randomIO
  let name = "sayland-test-" <> show n
      size = 64 :: Int
      pixels = fromIntegral <$> [1 .. size] :: [Word8]
      sharedMemory = shmOpen name (ShmOpenFlags True True False True) (unionFileModes ownerReadMode ownerWriteMode)
  ClientEnv env <- ask
  liftIO . bracket sharedMemory (const $ shmUnlink name) $ \fd -> usingReaderT (ClientEnv env) $ do
    liftIO $ setFdSize fd (fromIntegral size)
    pool <- newObject shm $ \i -> Request_wl_shm_create_pool i (WlFd fd) (fromIntegral size)
    handle <- liftIO $ fdToHandle fd
    liftIO $ BS.hPut handle (BS.pack pixels) >> hFlush handle
    roundtrip s
    compositorPool <- liftIO $ compositorObject s pool.wlid
    case compositorPool of
      Nothing -> liftIO $ assertFailure "the compositor has no wl_shm_pool"
      Just (Wl_shm_pool{ptr}) -> do
        mapped <- readIORef ptr
        liftIO $ peekArray size (castPtr mapped) >>= (@?= pixels)

-- | A request this side checks and rejects throws `InvalidMessage`, and the connection carries on as if it was never made.
rejectsOwnRequest :: Assertion
rejectsOwnRequest = withSession $ \s -> do
  oid <- newObjectID
  rejected <-
    (Nothing <$ sendMsg s.registry (Request_wl_registry_bind 999 (WlNewId [wl|wl_compositor|] 1 oid)))
      `catchW` \(e :: InvalidMessage) -> pure (Just e)
  liftIO $ assertBool "the bind of a global that was never advertised was sent" (isJust rejected)
  roundtrip s
  liftIO $ do
    ended <- tryReadMVar s.clientLoopEnded
    assertBool ("the connection ended: " <> show ended) (isNothing ended)
    compositorObject @Wl_compositor s (TObjectID oid) >>= assertBool "the compositor has the object" . isNothing

-- | A request that breaks the protocol, sent without this side's checks, ends the connection with a @wl_display.error@.
reportsViolation :: Assertion
reportsViolation = withSession $ \s -> do
  ClientEnv env <- ask
  globals <- readIORef env.globals
  oid <- newObjectID
  case [name | (name, (iface, _)) <- Map.toList globals, iface == [wl|wl_compositor|]] of
    [] -> liftIO $ assertFailure "wl_compositor is not advertised"
    name : _ -> sendMessage (Request_wl_registry_bind (coerce name) (WlNewId [wl|wl_compositor|] 0 oid)) s.registry.wlid
  liftIO $ expectProtocolError s Err_invalid_method

-- | A message the compositor cannot decode ends the connection with a @wl_display.error@.
reportsMalformed :: Assertion
reportsMalformed = withSession $ \s -> do
  ClientEnv env <- ask
  -- wl_display.sync, with the null object for its callback.
  takeMVar env.writeLock
  liftIO $ sendRaw env.socket (coerce s.display.wlid) 0 (putWlUInt 0)
  liftIO $ expectProtocolError s Err_invalid_method
  putMVar env.writeLock ()

-- Harness {{{

-- | A client connected to a compositor of its own, with the registry's globals already received.
data Session = Session
  { compositor :: ServerEnvironment
  , display :: Wl_display
  , registry :: Wl_registry
  , clientLoopEnded :: MVar SomeException
  -- ^ Filled when the client's event loop ends, which it only does with an exception.
  }

{- | Run a client against a compositor. Both from this library.
A test that does not finish in time fails which catches hangs.
-}
withSession :: (Session -> Wayland Client ()) -> Assertion
withSession test = do
  tmp <- getTemporaryDirectory
  n :: Word32 <- randomIO
  let dir = tmp <> "/sayland-test-" <> show n
      path = dir <> "/wayland-0"
  finished <- timeout 10_000_000
    . bracket_ (createDirectory dir) (removeDirectoryRecursive dir)
    $ bracket (listenOn path) close
    $ \sock -> do
      compositor <- serverEnvironment sock path
      -- The client disconnecting ends the compositor's connection to it, which the compositor then drops.
      let compositorDropsClients = atomically $ readTVar compositor.clients >>= check . Map.null
      bracket (forkIO $ listenForClients compositor) (\t -> compositorDropsClients >> killThread t) $ \_ -> do
        ClientEnv client <- waylandConnect table path
        clientLoopEnded <- newEmptyMVar
        bracket (forkClientLoop client clientLoopEnded) (\t -> killThread t >> close client.socket) $ \_ ->
          usingReaderT (ClientEnv client) $ do
            display <- getWlDisplay
            registry <- newObject display Request_wl_display_get_registry
            let session = Session{compositor, display, registry, clientLoopEnded}
            roundtrip session
            test session
  maybe (assertFailure "timed out") pure finished
  where
    forkClientLoop :: ClientEnvironment Client -> MVar SomeException -> IO ThreadId
    forkClientLoop client ended = forkFinally (usingReaderT (ClientEnv client) $ clientLoop client.socket) (either (void . tryPutMVar ended) pure)

listenOn :: FilePath -> IO Socket
listenOn path = do
  sock <- socket AF_UNIX Stream defaultProtocol
  bind sock (SockAddrUnix path)
  listen sock 5
  pure sock

serverEnvironment :: Socket -> FilePath -> IO ServerEnvironment
serverEnvironment sock socketPath = do
  clients <- newTVarIO Map.empty
  eventHandlers <- newIORef []
  clientSerial <- newTVarIO 0
  pure ServerEnvironment{socket = sock, socketPath, clients, interfaceTable = Map.fromList table, eventHandlers, clientSerial}

-- | Wait until the compositor has handled every request sent before: Wayland's own round trip.
roundtrip :: Session -> Wayland Client ()
roundtrip s = newObject s.display Request_wl_display_sync >>= takeMVar . (.done)

-- | Get an object from the compositor from its only client.
compositorObject :: forall i. (Typeable i) => Session -> TObjectID i -> IO (Maybe i)
compositorObject s (TObjectID oid) =
  readTVarIO s.compositor.clients
    >>= ( \case
            [client] -> (proxyInterface <=< Map.lookup oid) <$> readIORef client.objects
            clients -> assertFailure $ "the compositor has " <> show (length clients) <> " clients, not 1"
        )
    . Map.elems

-- | The client's connection ends with a @wl_display.error@ about the display, with this code.
expectProtocolError :: Session -> Enum_wl_display_error -> Assertion
expectProtocolError s code = do
  ended <- takeMVar s.clientLoopEnded
  case fromException ended of
    Just (e :: ProtocolError) -> do
      e.object @?= Just (coerce s.display.wlid)
      e.code @?= errorCode code
    Nothing -> assertFailure $ "the connection ended with something else: " <> displayException ended

-- }}}

-- vim: foldmethod=marker
