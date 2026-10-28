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
import Sayland.Internal.Core (TObjectID (..), getConnectionEnv, sendMessage)
import System.Directory (createDirectory, getTemporaryDirectory, removeDirectoryRecursive)
import System.Posix (ShmOpenFlags (..), fdToHandle, ownerReadMode, ownerWriteMode, setFdSize, shmOpen, shmUnlink, unionFileModes)
import System.Random (randomIO)
import System.Timeout (timeout)
import Test.Tasty
import Test.Tasty.HUnit
import Utils

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
    , testCase "a surface keeps one extension per type" surfaceExtensions
    , testCase "a synchronized subsurface's commits wait for its parent's" syncedSubsurfaceWaits
    , testCase "a synchronized subsurface's commits are merged in the order they were committed" syncedCommitsMergeInOrder
    , testCase "nested synchronized subsurfaces' commits are merged in order and applied with the outermost parent" nestedSyncedCommitsMerge
    , testCase "a desynchronized subsurface's commits are applied right away" desyncedSubsurfaceApplies
    , testCase "an update waits on the compositor until its constraints hold" constrainedUpdateWaits
    , testCase "a synchronized subsurface's constraints hold back its parent's update" syncedConstraintsHoldParent
    , testCase "a fifo barrier holds the updates that wait for it until the compositor clears it" fifoBarrierHolds
    , testCase "a synchronized subsurface ignores wait_barrier" syncedSubsurfaceIgnoresFifo
    , testCase "the compositor rejects a second fifo object for a surface" rejectsSecondFifo
    , testCase "withEventLoop returns what the client returns" eventLoopReturns
    , testCase "withEventLoop throws the event loop's error to the client" eventLoopThrows
    , testCase "untilDisconnected returns when the compositor hangs up" untilDisconnectedReturns
    ]

table :: ProtocolTable
table = waylandTable <> xdg_shellTable <> fifo_v1Table

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

-- | Only one extension per type
surfaceExtensions :: Assertion
surfaceExtensions = withSession $ \s -> do
  wl_compositor <- bindToInterface @Wl_compositor s.registry
  surface <- newObject wl_compositor Request_wl_compositor_create_surface
  found <- atomically $ do
    made <- surfaceExtension surface (pure (1 :: Int))
    again <- surfaceExtension surface (pure (2 :: Int))
    other <- surfaceExtension surface (pure ("text" :: Text))
    missing <- lookupSurfaceExtension @Bool surface
    pure (made, again, other, missing)
  liftIO $ found @?= (1, 1, "text", Nothing)

-- | A synchronized subsurface's commits are sync content updates: merged into one, and applied with its parent's next commit.
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
    syncUpdates <- (.cuSyncUpdates) <$> readIORef compositorParent.pendingState
    length syncUpdates @?= 1
  sendMsg parent Request_wl_surface_commit
  scales s child >>= liftIO . (@?= (3, 3))

-- | A synchronized subsurface's commits are merged in the order they were committed: the later one's state replaces
-- the earlier one's, offsets add up and the damage of both is kept. Applying them one after the other instead would
-- lose the first one's damage.
syncedCommitsMergeInOrder :: Assertion
syncedCommitsMergeInOrder = withSession $ \s -> do
  (parent, child, _) <- withSubsurface s
  sendMsg child $ Request_wl_surface_set_buffer_scale 2
  sendMsg child $ Request_wl_surface_offset 1 1
  sendMsg child $ Request_wl_surface_damage 1 1 1 1
  sendMsg child Request_wl_surface_commit
  sendMsg child $ Request_wl_surface_set_buffer_scale 3
  sendMsg child $ Request_wl_surface_offset 10 10
  sendMsg child $ Request_wl_surface_damage 2 2 1 1
  sendMsg child Request_wl_surface_commit
  sendMsg parent Request_wl_surface_commit
  (client, compositor) <- states s child
  liftIO . for_ [client, compositor] $ \current -> do
    current.sBufferScale @?= 3
    current.sBufferOffset @?= (11, 11)
    sort ((.position) <$> current.sDamage) @?= [(1, 1), (2, 2)]

-- | A synchronized subsurface of a synchronized subsurface: each commit of the middle surface carries the inner one's,
-- and merging the middle one's merges the inner one's in order too. Nothing is applied before the outermost parent commits.
nestedSyncedCommitsMerge :: Assertion
nestedSyncedCommitsMerge = withSession $ \s -> do
  wl_compositor <- bindToInterface @Wl_compositor s.registry
  subcompositor <- bindToInterface @Wl_subcompositor s.registry
  root <- newObject wl_compositor Request_wl_compositor_create_surface
  middle <- newObject wl_compositor Request_wl_compositor_create_surface
  inner <- newObject wl_compositor Request_wl_compositor_create_surface
  void . newObject subcompositor $ \i -> Request_wl_subcompositor_get_subsurface i middle.wlid root.wlid
  void . newObject subcompositor $ \i -> Request_wl_subcompositor_get_subsurface i inner.wlid middle.wlid
  for_ [2, 3] $ \scale -> do
    sendMsg inner $ Request_wl_surface_set_buffer_scale scale
    sendMsg inner Request_wl_surface_commit
    sendMsg middle Request_wl_surface_commit
  scales s inner >>= liftIO . (@?= (1, 1))
  sendMsg root Request_wl_surface_commit
  scales s inner >>= liftIO . (@?= (3, 3))

-- | A desynchronized subsurface's commits are desync content updates: applied right away, not with its parent's commit.
desyncedSubsurfaceApplies :: Assertion
desyncedSubsurfaceApplies = withSession $ \s -> do
  (_, child, subsurface) <- withSubsurface s
  sendMsg subsurface Request_wl_subsurface_set_desync
  sendMsg child $ Request_wl_surface_set_buffer_scale 2
  sendMsg child Request_wl_surface_commit
  scales s child >>= liftIO . (@?= (2, 2))

-- | An update whose constraint does not hold waits in the compositor's queue, and the update committed after it waits
-- behind it. Once the constraint holds, a flush applies both in order and runs their apply hooks once.
-- The update is a desync content update, so only its constraint's desync check holds it back.
-- The client applies its commits right away.
constrainedUpdateWaits :: Assertion
constrainedUpdateWaits = withSession $ \s -> do
  wl_compositor <- bindToInterface @Wl_compositor s.registry
  surface <- newObject wl_compositor Request_wl_compositor_create_surface
  roundtrip
  compositorSurface <- onCompositor s surface
  ready <- newTVarIO False
  applied <- newTVarIO (0 :: Int)
  let constraint = Constraint{whenDesync = const (readTVar ready), whenSync = const (pure True)}
  atomicModifyIORef' compositorSurface.pendingState $ \cu -> (cu{cuConstraints = [constraint], cuOnApply = [modifyTVar' applied (+ 1)]}, ())
  sendMsg surface $ Request_wl_surface_set_buffer_scale 2
  sendMsg surface Request_wl_surface_commit
  sendMsg surface $ Request_wl_surface_set_buffer_scale 3
  sendMsg surface Request_wl_surface_commit
  scales s surface >>=@?== (3, 1)
  readTVarIO compositorSurface.queue >>= (liftIO . (@?= 2)) . length
  liftIO . asCompositor s $ do
    env <- getConnectionEnv
    atomically $ writeTVar ready True >> flushContentUpdates env compositorSurface
  scales s surface >>=@?== (3, 3)
  readTVarIO compositorSurface.queue >>= (liftIO . (@?= 0)) . length
  readTVarIO applied >>=@?== 1

-- | The constraints of a synchronized subsurface's sync content update hold back its parent's update, as the two are
-- applied together. A constraint can pass for sync content updates, as FIFO's must.
syncedConstraintsHoldParent :: Assertion
syncedConstraintsHoldParent = withSession $ \s -> do
  (parent, child, _) <- withSubsurface s
  compositorParent <- onCompositor s parent
  compositorChild <- onCompositor s child
  ready <- newTVarIO False
  let constrainChild c = atomicModifyIORef' compositorChild.pendingState $ \cu -> (cu{cuConstraints = [c]}, ())
  -- Passes for a sync content update, so the parent's update is applied right away.
  constrainChild Constraint{whenDesync = const (readTVar ready), whenSync = const (pure True)}
  sendMsg child $ Request_wl_surface_set_buffer_scale 2
  sendMsg child Request_wl_surface_commit
  sendMsg parent Request_wl_surface_commit
  scales s child >>=@?== (2, 2)
  -- Checked for a sync content update, so the parent's update waits for it.
  constrainChild Constraint{whenDesync = const (readTVar ready), whenSync = const (readTVar ready)}
  sendMsg child $ Request_wl_surface_set_buffer_scale 3
  sendMsg child Request_wl_surface_commit
  sendMsg parent $ Request_wl_surface_set_buffer_scale 2
  sendMsg parent Request_wl_surface_commit
  scales s child >>=@?== (3, 2)
  scales s parent >>=@?== (2, 1)
  liftIO . asCompositor s $ do
    env <- getConnectionEnv
    atomically $ writeTVar ready True >> flushContentUpdates env compositorParent
  scales s child >>=@?== (3, 3)
  scales s parent >>=@?== (2, 2)

-- | Each update waits for the barrier, then sets it.
-- The barrier starts down, so the first is applied right away, and the compositor applies one more per clear.
fifoBarrierHolds :: Assertion
fifoBarrierHolds = withSession $ \s -> do
  wl_compositor <- bindToInterface @Wl_compositor s.registry
  surface <- newObject wl_compositor Request_wl_compositor_create_surface
  fifo <- withFifo s surface
  compositorSurface <- onCompositor s surface
  let clearBarrier =
        liftIO . asCompositor s $ getConnectionEnv >>= \env -> atomically (clearFifoBarrier env compositorSurface)

  -- each update does what a fifo client usually does: wait for the previous frame's barrier, then raise its own
  for_ [2, 3, 4] $ \scale -> do
    sendMsg fifo Request_wp_fifo_v1_wait_barrier -- adds a constraint to the pending cuConstraints that waits while the barrier is up
    sendMsg fifo Request_wp_fifo_v1_set_barrier -- adds `writeTVar barrier True` to the pending cuOnApply
    sendMsg surface $ Request_wl_surface_set_buffer_scale scale
    sendMsg surface Request_wl_surface_commit -- the surface isn't a subsurface, so its update is queued and flushed on the compositor

  -- 2: the barrier was never raised, so its constraint passes. it is applied, and its cuOnApply raises the barrier.
  -- 3: the barrier is up, so it waits in the queue.
  -- 4: waits behind 3.

  -- the client applies every commit right away, so only the compositor's scale shows the barrier at work.
  scales s surface >>=@?== (4, 2)
  clearBarrier -- 3's constraint passes and it is applied. in the same transaction its cuOnApply raises the barrier again, so 4 keeps waiting
  scales s surface >>=@?== (4, 3)
  clearBarrier -- 4 is applied and raises the barrier, with nothing left to wait for it
  scales s surface >>=@?== (4, 4)

-- | The child raises its barrier, then waits for it. The wait is ignored, as the child is synchronized.
syncedSubsurfaceIgnoresFifo :: Assertion
syncedSubsurfaceIgnoresFifo = withSession $ \s -> do
  (parent, child, _) <- withSubsurface s
  fifo <- withFifo s child
  sendMsg fifo Request_wp_fifo_v1_set_barrier -- adds `writeTVar barrier True` to the child's pending cuOnApply
  sendMsg child Request_wl_surface_commit -- the child is synchronized, so its update is cached in the parent's pending cuSyncUpdates, not applied
  sendMsg parent Request_wl_surface_commit -- the parent's update is applied, and the child's with it. the child's cuOnApply runs and the barrier goes up.
  roundtrip -- wait until the compositor has handled both commits
  barrier <- onCompositor s child >>= atomically . (readTVar <=< fifoBarrier) -- read the child's barrier on the compositor
  liftIO $ assertBool "the child's barrier is down but it should be up" barrier
  sendMsg fifo Request_wp_fifo_v1_wait_barrier -- adds a constraint to the child's pending cuConstraints that waits while the barrier is up
  sendMsg child $ Request_wl_surface_set_buffer_scale 3 -- Request a state update, compositor's scale only becomes 3 once this update is applied
  sendMsg child Request_wl_surface_commit -- cached in the parent's pending cuSyncUpdates again, so its constraint is checked with whenSync
  sendMsg parent Request_wl_surface_commit -- whenSync passes even though the barrier is up, so nothing holds the parent's update back and both are applied
  scales s child >>=@?== (3, 3) -- the client applies right away; the compositor only has 3 if the barrier was ignored

-- | A @get_fifo@ sent without for a surface that already has a fifo object ends the connection.
rejectsSecondFifo :: Assertion
rejectsSecondFifo = withSession $ \s -> do
  wl_compositor <- bindToInterface @Wl_compositor s.registry
  surface <- newObject wl_compositor Request_wl_compositor_create_surface
  manager <- bindToInterface @Wp_fifo_manager_v1 s.registry
  _ <- newObject manager $ \i -> Request_wp_fifo_manager_v1_get_fifo i surface.wlid
  oid <- newObjectID
  sendMessage (Request_wp_fifo_manager_v1_get_fifo (TObjectID $ FromClient oid) surface.wlid) manager.wlid
  liftIO $ do
    ended <- takeMVar s.clientLoopEnded
    case fromException ended of
      Just (e :: ProtocolError) -> do
        e.object @?= Just (toObjectID manager.wlid)
        e.code @?= errorCode Enum_wp_fifo_manager_v1_error_already_exists
      Nothing -> assertFailure $ "the connection ended with something else: " <> displayException ended

-- | A fifo object for a surface, made on the compositor too.
withFifo :: Session -> Wl_surface -> Wayland Client Wp_fifo_v1
withFifo s surface = do
  manager <- bindToInterface @Wp_fifo_manager_v1 s.registry
  fifo <- newObject manager $ \i -> Request_wp_fifo_manager_v1_get_fifo i surface.wlid
  roundtrip
  pure fifo

-- | A surface with a subsurface, synchronized as subsurfaces start out, made on the compositor too.
withSubsurface :: Session -> Wayland Client (Wl_surface, Wl_surface, Wl_subsurface)
withSubsurface s = do
  wl_compositor <- bindToInterface @Wl_compositor s.registry
  subcompositor <- bindToInterface @Wl_subcompositor s.registry
  parent <- newObject wl_compositor Request_wl_compositor_create_surface
  child <- newObject wl_compositor Request_wl_compositor_create_surface
  subsurface <- newObject subcompositor $ \i -> Request_wl_subcompositor_get_subsurface i child.wlid parent.wlid
  roundtrip
  pure (parent, child, subsurface)

-- | The compositor's object for a surface the client made.
onCompositor :: Session -> Wl_surface -> Wayland Client Wl_surface
onCompositor s surface = liftIO $ compositorObject s surface.wlid >>= maybe (assertFailure "the compositor has no wl_surface") pure

-- | A surface's current state on the client and on the compositor, after a roundtrip.
states :: Session -> Wl_surface -> Wayland Client (SurfaceState, SurfaceState)
states s surface = do
  roundtrip
  client <- readTVarIO surface.state
  compositor <- onCompositor s surface >>= readTVarIO . (.state)
  pure (client, compositor)

-- | The buffer scale of a surface's current state on the client and on the compositor, after a roundtrip.
scales :: Session -> Wl_surface -> Wayland Client (Int, Int)
scales s surface = do
  roundtrip
  client <- (.sBufferScale) <$> readTVarIO surface.state
  compositor <-
    liftIO $ compositorObject s surface.wlid >>= \case
      Nothing -> assertFailure "the compositor has no wl_surface"
      Just compositorSurface -> (.sBufferScale) <$> readTVarIO compositorSurface.state
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
