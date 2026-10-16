{-# LANGUAGE QuasiQuotes #-}

-- | Description : A compositor and a client from this library, talking over a real socket.
module Integration (tests) where

import Control.Concurrent (ThreadId, forkFinally, forkIO, killThread, threadDelay)
import Control.Concurrent.STM (check)
import Control.Exception (bracket, bracket_, try)
import Data.ByteString qualified as BS
import Data.Map qualified as Map
import Foreign (castPtr, peekArray)
import GHC.IO.Handle (hFlush)
import Network.Socket (ShutdownCmd (ShutdownBoth), close, shutdown)
import Relude hiding (hFlush)
import Sayland
import Sayland.Internal.Core (TObjectID (..), sendMessage)
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
    , testCase "Client and Server IDs don't collide" serverIDsDoNotCollide
    , testCase "the compositor rejects an ID that is still in use" rejectsReusedID
    , testCase "the compositor rejects a new ID that skips ahead" rejectsSkippedID
    , testCase "new IDs reach the compositor in order when threads create objects at once" concurrentNewIDs
    , testCase "a commit applies the pending state" commitAppliesState
    , testCase "a synchronized subsurface's commits wait for its parent's" syncedSubsurfaceWaits
    , testCase "withEventLoop returns what the client returns" eventLoopReturns
    , testCase "withEventLoop throws the event loop's error to the client" eventLoopThrows
    , testCase "untilDisconnected returns when the compositor hangs up" untilDisconnectedReturns
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
  roundtrip
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
    pool <- newObject shm $ \i -> Request_wl_shm_create_pool i fd (fromIntegral size)
    handle <- liftIO $ fdToHandle fd
    liftIO $ BS.hPut handle (BS.pack pixels) >> hFlush handle
    roundtrip
    compositorPool <- liftIO $ compositorObject s pool.wlid
    case compositorPool of
      Nothing -> liftIO $ assertFailure "the compositor has no wl_shm_pool"
      Just (Wl_shm_pool{ptr}) -> do
        mapped <- readIORef ptr
        liftIO $ peekArray size (castPtr mapped) >>= (@?= pixels)

-- | A request this side checks and rejects throws `InvalidMessage`, and the connection carries on as if it was never made.
rejectsOwnRequest :: Assertion
rejectsOwnRequest = withSession $ \s -> do
  rejected <-
    (Nothing <$ withNewObjectID (sendMsg s.registry . Request_wl_registry_bind 999 . WlNewId [wl|wl_compositor|] 1))
      `catchW` \(e :: InvalidMessage) -> pure (Just e)
  liftIO $ assertBool "the bind of a global that was never advertised was sent" (isJust rejected)
  roundtrip
  liftIO $ do
    ended <- tryReadMVar s.clientLoopEnded
    assertBool ("the connection ended: " <> show ended) (isNothing ended)

-- | A request that breaks the protocol, sent without this side's checks, ends the connection with a @wl_display.error@.
reportsViolation :: Assertion
reportsViolation = withSession $ \s -> do
  ClientEnv env <- ask
  globals <- readIORef env.globals
  oid <- newObjectID
  case [name | (name, (iface, _)) <- Map.toList globals, iface == [wl|wl_compositor|]] of
    [] -> liftIO $ assertFailure "wl_compositor is not advertised"
    name : _ -> sendMessage (Request_wl_registry_bind name (WlNewId [wl|wl_compositor|] 0 oid)) s.registry.wlid
  liftIO $ expectProtocolError s Err_invalid_method

-- | A message the compositor cannot decode ends the connection with a @wl_display.error@.
reportsMalformed :: Assertion
reportsMalformed = withSession $ \s -> do
  ClientEnv env <- ask
  -- wl_display.sync, with the null object for its callback.
  takeMVar env.writeLock
  liftIO $ sendRaw env.connectionSocket (toObjectID s.display.wlid) 0 (putWlUInt 0)
  liftIO $ expectProtocolError s Err_invalid_method
  putMVar env.writeLock ()

-- | Like libwayland, the compositor ends the connection when a new ID skips the next one the client may use.
rejectsSkippedID :: Assertion
rejectsSkippedID = withSession $ \s -> do
  _skipped <- newObjectID
  oid <- newObjectID
  sendMessage (Request_wl_display_get_registry . TObjectID $ FromClient oid) s.display.wlid
  liftIO $ expectProtocolError s Err_invalid_object

-- | Threads creating objects at the same time still send their new IDs in the order they were allocated.
concurrentNewIDs :: Assertion
concurrentNewIDs = withSession $ \s -> do
  wl_compositor <- bindToInterface @Wl_compositor s.registry
  env <- ask
  let createSurfaces = usingReaderT env . replicateM_ 100 $ newObject wl_compositor Request_wl_compositor_create_surface
  threads <- liftIO . replicateM 8 $ do
    done <- newEmptyMVar
    void $ forkFinally createSurfaces (putMVar done)
    pure done
  liftIO $ mapM_ (takeMVar >=> either (assertFailure . displayException) pure) threads
  roundtrip
  liftIO $ tryReadMVar s.clientLoopEnded >>= \ended -> assertBool ("the connection ended: " <> show ended) (isNothing ended)

-- | A commit applies the surface's pending state to its current state, on both sides.
commitAppliesState :: Assertion
commitAppliesState = withSession $ \s -> do
  wl_compositor <- bindToInterface @Wl_compositor s.registry
  surface <- newObject wl_compositor Request_wl_compositor_create_surface
  sendMsg surface $ Request_wl_surface_set_buffer_scale 2
  scales s surface >>= liftIO . (@?= (1, 1))
  sendMsg surface Request_wl_surface_commit
  scales s surface >>= liftIO . (@?= (2, 2))

-- | A synchronized subsurface's commits are cached, merged into one, and applied with its parent's next commit.
syncedSubsurfaceWaits :: Assertion
syncedSubsurfaceWaits = withSession $ \s -> do
  wl_compositor <- bindToInterface @Wl_compositor s.registry
  subcompositor <- bindToInterface @Wl_subcompositor s.registry
  parent <- newObject wl_compositor Request_wl_compositor_create_surface
  child <- newObject wl_compositor Request_wl_compositor_create_surface
  void . newObject subcompositor $ \i -> Request_wl_subcompositor_get_subsurface i child.wlid parent.wlid
  sendMsg child $ Request_wl_surface_set_buffer_scale 2
  sendMsg child Request_wl_surface_commit
  sendMsg child $ Request_wl_surface_set_buffer_scale 3
  sendMsg child Request_wl_surface_commit
  scales s child >>= liftIO . (@?= (1, 1))
  liftIO $ do
    Just compositorParent <- compositorObject s parent.wlid
    cached <- (.cuSlaveCUs) <$> readIORef compositorParent.pendingState
    length cached @?= 1
  sendMsg parent Request_wl_surface_commit
  scales s child >>= liftIO . (@?= (3, 3))

-- | The buffer scale of a surface's current state on the client and on the compositor, after a roundtrip.
scales :: Session -> Wl_surface -> Wayland Client (Int, Int)
scales s surface = do
  roundtrip
  client <- (.sBufferScale) <$> readIORef surface.state
  compositor <-
    liftIO $ compositorObject s surface.wlid >>= \case
      Nothing -> assertFailure "the compositor has no wl_surface"
      Just compositorSurface -> (.sBufferScale) <$> readIORef compositorSurface.state
  pure (client, compositor)

-- | The client's result comes back, after the event loop delivered the replies the client waited for.
eventLoopReturns :: Assertion
eventLoopReturns = withCompositor $ \path _ -> do
  result <- bracket (waylandConnect table path) waylandClose . runReaderT . withEventLoop $ do
    roundtrip
    pure (42 :: Int)
  result @?= 42

-- | The protocol error that ends the event loop interrupts the client, which would otherwise wait forever.
eventLoopThrows :: Assertion
eventLoopThrows = withCompositor $ \path _ -> do
  result <- try . bracket (waylandConnect table path) waylandClose . runReaderT . withEventLoop $ do
    display <- getWlDisplay
    registry <- newObject display Request_wl_display_get_registry
    -- A second registry with the same ID breaks the protocol.
    sendMessage (Request_wl_display_get_registry registry.wlid) display.wlid
    untilDisconnected
  case result of
    Left (e :: ProtocolError) -> e.code @?= errorCode Err_invalid_object
    Right () -> assertFailure "the client finished without the event loop's error"

-- | When the compositor ends the connection, `untilDisconnected` returns instead of throwing.
untilDisconnectedReturns :: Assertion
untilDisconnectedReturns = withCompositor $ \path compositor ->
  bracket (waylandConnect table path) waylandClose . runReaderT . withEventLoop $ do
    roundtrip
    -- Hang up after the client is waiting. A hangup before would be thrown at the client like any other.
    let hangUp = threadDelay 100_000 >> readTVarIO compositor.clients >>= mapM_ (\client -> shutdown client.connectionSocket ShutdownBoth)
    void . liftIO $ forkIO hangUp
    untilDisconnected

-- Harness {{{

-- | A client connected to a compositor of its own, with the registry's globals already received.
data Session = Session
  { compositor :: ServerEnv
  , display :: Wl_display
  , registry :: Wl_registry
  , clientLoopEnded :: MVar SomeException
  -- ^ Filled when the client's event loop ends, which it only does with an exception.
  }

-- | An object the compositor creates gets an ID from the server's range and does not replace one of the client's.
serverIDsDoNotCollide :: Assertion
serverIDsDoNotCollide = withSession $ \s -> do
  seat <- bindToInterface @Wl_seat s.registry
  manager <- bindToInterface @Wl_data_device_manager s.registry
  device <- newObject manager $ \i -> Request_wl_data_device_manager_get_data_device i seat.wlid
  roundtrip
  offer <- liftIO . asCompositor s $ do
    compositorDevice <- getInterface device.wlid >>= maybe (error "the compositor has no wl_data_device") pure
    newObject compositorDevice Event_wl_data_device_data_offer
  roundtrip
  liftIO $ assertBool "the offer's ID is not in the server's range" (fromAnyObjectID (toObjectID offer.wlid) >= 0xff000000)
  getInterface s.registry.wlid >>= liftIO . assertBool "the client's registry was replaced" . isJust
  void $ bindToInterface @Wl_compositor s.registry -- the registry still works
  roundtrip

-- | A new object with the ID of an object that still exists ends the connection, instead of replacing that object.
rejectsReusedID :: Assertion
rejectsReusedID = withSession $ \s -> do
  sendMessage (Request_wl_display_get_registry s.registry.wlid) s.display.wlid
  liftIO $ expectProtocolError s Err_invalid_object

-- | Run a client against a compositor. Both from this library.
withSession :: (Session -> Wayland Client ()) -> Assertion
withSession test = withCompositor $ \path compositor -> do
  ClientEnv client <- waylandConnect table path
  clientLoopEnded <- newEmptyMVar
  bracket (forkClientLoop client clientLoopEnded) (\t -> killThread t >> close client.connectionSocket) $ \_ ->
    usingReaderT (ClientEnv client) $ do
      display <- getWlDisplay
      registry <- newObject display Request_wl_display_get_registry
      let session = Session{compositor, display, registry, clientLoopEnded}
      roundtrip
      test session
  where
    forkClientLoop :: ConnectionEnv Client -> MVar SomeException -> IO ThreadId
    forkClientLoop client ended = forkFinally (usingReaderT (ClientEnv client) $ clientLoop client.connectionSocket) (either (void . tryPutMVar ended) pure)

-- | Run a compositor from this library on a socket of its own, and something given the socket's path.
-- Waits for the compositor to drop its clients after, so they must disconnect.
-- A test that does not finish in time fails which catches hangs.
withCompositor :: (FilePath -> ServerEnv -> IO ()) -> Assertion
withCompositor act = do
  tmp <- getTemporaryDirectory
  n :: Word32 <- randomIO
  let dir = tmp <> "/sayland-test-" <> show n
      path = dir <> "/wayland-0"
  finished <- timeout 10_000_000
    . bracket_ (createDirectory dir) (removeDirectoryRecursive dir)
    $ bracket (quiet <$> waylandListen table path) waylandServerClose
    $ \compositor -> do
      -- The client disconnecting ends the compositor's connection to it, which the compositor then drops.
      let compositorDropsClients = atomically $ readTVar compositor.clients >>= check . Map.null
      bracket (forkIO $ listenForClients compositor) (\t -> compositorDropsClients >> killThread t) . const $ act path compositor
  maybe (assertFailure "timed out") pure finished

-- | Silence the compositor's logger, unless @WAYLAND_DEBUG@ asks for the server. Some tests break the protocol on purpose,
-- and the errors the compositor logs for that would garble the test report.
quiet :: ServerEnv -> ServerEnv
quiet env = if env.serverLogger.minLevel == LogDebug then env else env{serverLogger = silentLogger}

-- | Run something as the compositor, on its connection to its only client.
asCompositor :: Session -> Wayland Server a -> IO a
asCompositor s act =
  readTVarIO s.compositor.clients >>= \clients -> case Map.toList clients of
    [(serial, client)] -> usingReaderT (ClientServerEnv s.compositor client serial) act
    _ -> assertFailure $ "the compositor has " <> show (Map.size clients) <> " clients, not 1"

-- | Get an object from the compositor from its only client.
compositorObject :: (Typeable i) => Session -> TObjectID i -> IO (Maybe i)
compositorObject s = asCompositor s . getInterface

-- | The client's connection ends with a @wl_display.error@ about the display, with this code.
expectProtocolError :: Session -> Enum_wl_display_error -> Assertion
expectProtocolError s code = do
  ended <- takeMVar s.clientLoopEnded
  case fromException ended of
    Just (e :: ProtocolError) -> do
      e.object @?= Just (toObjectID s.display.wlid)
      e.code @?= errorCode code
    Nothing -> assertFailure $ "the connection ended with something else: " <> displayException ended

-- }}}

-- vim: foldmethod=marker
