{-# LANGUAGE QuasiQuotes #-}

-- | Description : A compositor and a client from this library, talking over a simulated socket.
--
-- The tests main has, run once each, with each thread running until it blocks. They show the dejafu branch behaves like
-- main. The race tests explore the interleavings.
module Integration (tests) where

import Control.Monad.Catch (bracket, try)
import Data.ByteString qualified as BS
import Data.Map qualified as Map
import Harness
import Sayland
import Sayland.Internal.Core (TObjectID (..), getConnectionEnv, sendMessage)
import Sayland.Kernel (Kernel, ShutdownCmd (ShutdownBoth), close, defaultKernelConfig, memfd, newKernel, peekBytes, pwrite, setFdSize, shutdown)
import Test.Tasty
import Test.Tasty.HUnit
import TestPrelude
import Utils

tests :: TestTree
tests =
  testGroup
    "Integration"
    [ testConc "the client binds globals the compositor advertises" bindsGlobals
    , testConc "a file descriptor reaches the compositor with its request" passesFd
    , testConc "a request the client rejects is never sent" rejectsOwnRequest
    , testConc "the compositor reports a protocol violation to the client" reportsViolation
    , testConc "the compositor reports a malformed message to the client" reportsMalformed
    , testConc "Client and Server IDs don't collide" serverIDsDoNotCollide
    , testConc "the compositor rejects an ID that is still in use" rejectsReusedID
    , testConc "the compositor rejects a new ID that skips ahead" rejectsSkippedID
    , testConc "new IDs reach the compositor in order when threads create objects at once" concurrentNewIDs
    , testConc "a commit applies the pending state" commitAppliesState
    , testConc "a surface keeps one extension per type" surfaceExtensions
    , testConc "a synchronized subsurface's commits wait for its parent's" syncedSubsurfaceWaits
    , testConc "a synchronized subsurface's commits are merged in the order they were committed" syncedCommitsMergeInOrder
    , testConc "nested synchronized subsurfaces' commits are merged in order and applied with the outermost parent" nestedSyncedCommitsMerge
    , testConc "a desynchronized subsurface's commits are applied right away" desyncedSubsurfaceApplies
    , testConc "an update waits on the compositor until its constraints hold" constrainedUpdateWaits
    , testConc "a synchronized subsurface's constraints hold back its parent's update" syncedConstraintsHoldParent
    , testConc "a fifo barrier holds the updates that wait for it until the compositor clears it" fifoBarrierHolds
    , testConc "a synchronized subsurface ignores wait_barrier" syncedSubsurfaceIgnoresFifo
    , testConc "the compositor rejects a second fifo object for a surface" rejectsSecondFifo
    , testConc "withEventLoop returns what the client returns" eventLoopReturns
    , testConc "withEventLoop throws the event loop's error to the client" eventLoopThrows
    , testConc "untilDisconnected returns when the compositor hangs up" untilDisconnectedReturns
    ]
  where
    testConc name = testCase name . runOnce

table :: ProtocolTable
table = waylandTable <> xdg_shellTable <> fifo_v1Table

-- | The client binds some globals. The objects it then creates exist on the compositor too.
bindsGlobals :: ConcIO ()
bindsGlobals = withSession $ \s -> do
  ClientEnv env <- ask
  advertised <- fmap fst . Map.elems <$> readIORef env.globals
  liftConc $ sort advertised @?== sort (fst <$> table)
  shm <- bindToInterface @Wl_shm s.registry
  wl_compositor <- bindToInterface @Wl_compositor s.registry
  surface <- newObject wl_compositor Request_wl_compositor_create_surface
  roundtrip
  liftConc $ do
    compositorObject s shm.wlid >>= assertTrue "the compositor has no wl_shm" . isJust
    compositorObject s surface.wlid >>= assertTrue "the compositor has no wl_surface" . isJust

-- | A pool's fd is sent alongside @create_pool@. The compositor maps the same memory the client writes.
passesFd :: ConcIO ()
passesFd = withSession $ \s -> do
  shm <- bindToInterface @Wl_shm s.registry
  let size = 64 :: Int
      pixels = BS.pack $ fromIntegral <$> [1 .. size]
  fd <- liftConc $ memfd s.kernel
  liftConc $ setFdSize s.kernel fd size
  pool <- newObject shm $ \i -> Request_wl_shm_create_pool i fd (fromIntegral size)
  liftConc $ pwrite s.kernel fd 0 pixels
  roundtrip
  compositorPool <- liftConc $ compositorObject s pool.wlid
  case compositorPool of
    Nothing -> liftConc $ failTest "the compositor has no wl_shm_pool"
    Just (Wl_shm_pool{ptr}) -> do
      mapped <- readIORef ptr
      liftConc $ peekBytes s.kernel mapped size >>=@?== pixels

-- | A request this side checks and rejects throws `InvalidMessage`, and the connection carries on as if it was never made.
rejectsOwnRequest :: ConcIO ()
rejectsOwnRequest = withSession $ \s -> do
  rejected <-
    (Nothing <$ withNewObjectID (sendMsg s.registry . Request_wl_registry_bind 999 . WlNewId [wl|wl_compositor|] 1))
      `catchW` \(e :: InvalidMessage) -> pure (Just e)
  liftConc $ assertTrue "the bind of a global that was never advertised was sent" (isJust rejected)
  roundtrip
  liftConc $ do
    ended <- tryReadMVar s.clientLoopEnded
    assertTrue ("the connection ended: " <> show ended) (isNothing ended)

-- | A request that breaks the protocol, sent without this side's checks, ends the connection with a @wl_display.error@.
reportsViolation :: ConcIO ()
reportsViolation = withSession $ \s -> do
  ClientEnv env <- ask
  globals <- readIORef env.globals
  oid <- newObjectID
  case [name | (name, (iface, _)) <- Map.toList globals, iface == [wl|wl_compositor|]] of
    [] -> liftConc $ failTest "wl_compositor is not advertised"
    name : _ -> sendMessage (Request_wl_registry_bind name (WlNewId [wl|wl_compositor|] 0 oid)) s.registry.wlid
  liftConc $ expectProtocolError s Err_invalid_method

-- | A message the compositor cannot decode ends the connection with a @wl_display.error@.
reportsMalformed :: ConcIO ()
reportsMalformed = withSession $ \s -> do
  ClientEnv env <- ask
  -- wl_display.sync, with the null object for its callback.
  takeMVar env.writeLock
  liftConc $ sendRaw env.connectionSocket (toObjectID s.display.wlid) 0 (putWlUInt 0)
  liftConc $ expectProtocolError s Err_invalid_method
  putMVar env.writeLock ()

-- | Like libwayland, the compositor ends the connection when a new ID skips the next one the client may use.
rejectsSkippedID :: ConcIO ()
rejectsSkippedID = withSession $ \s -> do
  _skipped <- newObjectID
  oid <- newObjectID
  sendMessage (Request_wl_display_get_registry . TObjectID $ FromClient oid) s.display.wlid
  liftConc $ expectProtocolError s Err_invalid_object

-- | Threads creating objects at the same time still send their new IDs in the order they were allocated.
concurrentNewIDs :: ConcIO ()
concurrentNewIDs = withSession $ \s -> do
  wl_compositor <- bindToInterface @Wl_compositor s.registry
  env <- ask
  let createSurfaces = usingReaderT env . replicateM_ 100 $ newObject wl_compositor Request_wl_compositor_create_surface
  threads <- liftConc . replicateM 8 $ do
    done <- newEmptyMVar
    void $ forkFinally createSurfaces (putMVar done)
    pure done
  liftConc $ mapM_ (takeMVar >=> either (failTest . displayException) pure) threads
  roundtrip
  liftConc $ tryReadMVar s.clientLoopEnded >>= \ended -> assertTrue ("the connection ended: " <> show ended) (isNothing ended)

-- | A commit applies the surface's pending state to its current state, on both sides.
commitAppliesState :: ConcIO ()
commitAppliesState = withSession $ \s -> do
  wl_compositor <- bindToInterface @Wl_compositor s.registry
  surface <- newObject wl_compositor Request_wl_compositor_create_surface
  sendMsg surface $ Request_wl_surface_set_buffer_scale 2
  scales s surface >>=@?== (1, 1)
  sendMsg surface Request_wl_surface_commit
  scales s surface >>=@?== (2, 2)

-- | Only one extension per type
surfaceExtensions :: ConcIO ()
surfaceExtensions = withSession $ \s -> do
  wl_compositor <- bindToInterface @Wl_compositor s.registry
  surface <- newObject wl_compositor Request_wl_compositor_create_surface
  found <- atomically $ do
    made <- surfaceExtension surface (pure (1 :: Int))
    again <- surfaceExtension surface (pure (2 :: Int))
    other <- surfaceExtension surface (pure ("text" :: Text))
    missing <- lookupSurfaceExtension @Bool surface
    pure (made, again, other, missing)
  liftConc $ found @?== (1, 1, "text", Nothing)

-- | A synchronized subsurface's commits are sync content updates: merged into one, and applied with its parent's next commit.
syncedSubsurfaceWaits :: ConcIO ()
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
  scales s child >>=@?== (1, 1)
  liftConc $ do
    Just compositorParent <- compositorObject s parent.wlid
    syncUpdates <- (.cuSyncUpdates) <$> readIORef compositorParent.pendingState
    length syncUpdates @?== 1
  sendMsg parent Request_wl_surface_commit
  scales s child >>=@?== (3, 3)

-- | A synchronized subsurface's commits are merged in the order they were committed: the later one's state replaces
-- the earlier one's, offsets add up and the damage of both is kept. Applying them one after the other instead would
-- lose the first one's damage.
syncedCommitsMergeInOrder :: ConcIO ()
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
  liftConc . for_ [client, compositor] $ \current -> do
    current.sBufferScale @?== 3
    current.sBufferOffset @?== (11, 11)
    sort ((.position) <$> current.sDamage) @?== [(1, 1), (2, 2)]

-- | A synchronized subsurface of a synchronized subsurface: each commit of the middle surface carries the inner one's,
-- and merging the middle one's merges the inner one's in order too. Nothing is applied before the outermost parent commits.
nestedSyncedCommitsMerge :: ConcIO ()
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
  scales s inner >>=@?== (1, 1)
  sendMsg root Request_wl_surface_commit
  scales s inner >>=@?== (3, 3)

-- | A desynchronized subsurface's commits are desync content updates: applied right away, not with its parent's commit.
desyncedSubsurfaceApplies :: ConcIO ()
desyncedSubsurfaceApplies = withSession $ \s -> do
  (_, child, subsurface) <- withSubsurface s
  sendMsg subsurface Request_wl_subsurface_set_desync
  sendMsg child $ Request_wl_surface_set_buffer_scale 2
  sendMsg child Request_wl_surface_commit
  scales s child >>=@?== (2, 2)

-- | An update whose constraint does not hold waits in the compositor's queue, and the update committed after it waits
-- behind it. Once the constraint holds, a flush applies both in order and runs their apply hooks once.
-- The update is a desync content update, so only its constraint's desync check holds it back.
-- The client applies its commits right away.
constrainedUpdateWaits :: ConcIO ()
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
  readTVarIO compositorSurface.queue >>= ((@?== 2)) . length
  liftConc . asCompositor s $ do
    env <- getConnectionEnv
    atomically $ writeTVar ready True >> flushContentUpdates env compositorSurface
  scales s surface >>=@?== (3, 3)
  readTVarIO compositorSurface.queue >>= ((@?== 0)) . length
  readTVarIO applied >>=@?== 1

-- | The constraints of a synchronized subsurface's sync content update hold back its parent's update, as the two are
-- applied together. A constraint can pass for sync content updates, as FIFO's must.
syncedConstraintsHoldParent :: ConcIO ()
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
  liftConc . asCompositor s $ do
    env <- getConnectionEnv
    atomically $ writeTVar ready True >> flushContentUpdates env compositorParent
  scales s child >>=@?== (3, 3)
  scales s parent >>=@?== (2, 2)

-- | Each update waits for the barrier, then sets it.
-- The barrier starts down, so the first is applied right away, and the compositor applies one more per clear.
fifoBarrierHolds :: ConcIO ()
fifoBarrierHolds = withSession $ \s -> do
  wl_compositor <- bindToInterface @Wl_compositor s.registry
  surface <- newObject wl_compositor Request_wl_compositor_create_surface
  fifo <- withFifo s surface
  compositorSurface <- onCompositor s surface
  let clearBarrier =
        liftConc . asCompositor s $ getConnectionEnv >>= \env -> atomically (clearFifoBarrier env compositorSurface)

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
syncedSubsurfaceIgnoresFifo :: ConcIO ()
syncedSubsurfaceIgnoresFifo = withSession $ \s -> do
  (parent, child, _) <- withSubsurface s
  fifo <- withFifo s child
  sendMsg fifo Request_wp_fifo_v1_set_barrier -- adds `writeTVar barrier True` to the child's pending cuOnApply
  sendMsg child Request_wl_surface_commit -- the child is synchronized, so its update is cached in the parent's pending cuSyncUpdates, not applied
  sendMsg parent Request_wl_surface_commit -- the parent's update is applied, and the child's with it. the child's cuOnApply runs and the barrier goes up.
  roundtrip -- wait until the compositor has handled both commits
  barrier <- onCompositor s child >>= atomically . (readTVar <=< fifoBarrier) -- read the child's barrier on the compositor
  liftConc $ assertTrue "the child's barrier is down but it should be up" barrier
  sendMsg fifo Request_wp_fifo_v1_wait_barrier -- adds a constraint to the child's pending cuConstraints that waits while the barrier is up
  sendMsg child $ Request_wl_surface_set_buffer_scale 3 -- Request a state update, compositor's scale only becomes 3 once this update is applied
  sendMsg child Request_wl_surface_commit -- cached in the parent's pending cuSyncUpdates again, so its constraint is checked with whenSync
  sendMsg parent Request_wl_surface_commit -- whenSync passes even though the barrier is up, so nothing holds the parent's update back and both are applied
  scales s child >>=@?== (3, 3) -- the client applies right away; the compositor only has 3 if the barrier was ignored

-- | A @get_fifo@ sent without for a surface that already has a fifo object ends the connection.
rejectsSecondFifo :: ConcIO ()
rejectsSecondFifo = withSession $ \s -> do
  wl_compositor <- bindToInterface @Wl_compositor s.registry
  surface <- newObject wl_compositor Request_wl_compositor_create_surface
  manager <- bindToInterface @Wp_fifo_manager_v1 s.registry
  _ <- newObject manager $ \i -> Request_wp_fifo_manager_v1_get_fifo i surface.wlid
  oid <- newObjectID
  sendMessage (Request_wp_fifo_manager_v1_get_fifo (TObjectID $ FromClient oid) surface.wlid) manager.wlid
  liftConc $ do
    ended <- takeMVar s.clientLoopEnded
    case fromException ended of
      Just (e :: ProtocolError) -> do
        e.object @?== Just (toObjectID manager.wlid)
        e.code @?== errorCode Enum_wp_fifo_manager_v1_error_already_exists
      Nothing -> failTest $ "the connection ended with something else: " <> displayException ended

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
    liftConc $ compositorObject s surface.wlid >>= \case
      Nothing -> failTest "the compositor has no wl_surface"
      Just compositorSurface -> (.sBufferScale) <$> readTVarIO compositorSurface.state
  pure (client, compositor)

-- | The client's result comes back, after the event loop delivered the replies the client waited for.
eventLoopReturns :: ConcIO ()
eventLoopReturns = withCompositor $ \kernel path _ -> do
  result <- bracket (waylandConnect kernel table path) waylandClose . runReaderT . withEventLoop $ do
    roundtrip
    pure (42 :: Int)
  result @?== (42 :: Int)

-- | The protocol error that ends the event loop interrupts the client, which would otherwise wait forever.
eventLoopThrows :: ConcIO ()
eventLoopThrows = withCompositor $ \kernel path _ -> do
  result <- try . bracket (waylandConnect kernel table path) waylandClose . runReaderT . withEventLoop $ do
    display <- getWlDisplay
    registry <- newObject display Request_wl_display_get_registry
    -- A second registry with the same ID breaks the protocol.
    sendMessage (Request_wl_display_get_registry registry.wlid) display.wlid
    untilDisconnected
  case result of
    Left (e :: ProtocolError) -> e.code @?== errorCode Err_invalid_object
    Right () -> failTest "the client finished without the event loop's error"

-- | When the compositor ends the connection, `untilDisconnected` returns instead of throwing.
untilDisconnectedReturns :: ConcIO ()
untilDisconnectedReturns = withCompositor $ \kernel path compositor ->
  bracket (waylandConnect kernel table path) waylandClose . runReaderT . withEventLoop $ do
    roundtrip
    -- Hang up after the client is waiting. A hangup before would be thrown at the client like any other.
    -- Each thread runs until it blocks, so this runs once the client waits.
    let hangUp = readTVarIO compositor.clients >>= mapM_ (\client -> shutdown client.connectionSocket ShutdownBoth)
    void . liftConc $ forkN "hang-up" hangUp
    untilDisconnected

-- Harness {{{

-- | An object the compositor creates gets an ID from the server's range and does not replace one of the client's.
serverIDsDoNotCollide :: ConcIO ()
serverIDsDoNotCollide = withSession $ \s -> do
  seat <- bindToInterface @Wl_seat s.registry
  manager <- bindToInterface @Wl_data_device_manager s.registry
  device <- newObject manager $ \i -> Request_wl_data_device_manager_get_data_device i seat.wlid
  roundtrip
  offer <- liftConc . asCompositor s $ do
    compositorDevice <- getInterface device.wlid >>= maybe (failTest "the compositor has no wl_data_device") pure
    newObject compositorDevice Event_wl_data_device_data_offer
  roundtrip
  assertTrue "the offer's ID is not in the server's range" (fromAnyObjectID (toObjectID offer.wlid) >= 0xff000000)
  getInterface s.registry.wlid >>= assertTrue "the client's registry was replaced" . isJust
  void $ bindToInterface @Wl_compositor s.registry -- the registry still works
  roundtrip

-- | A new object with the ID of an object that still exists ends the connection, instead of replacing that object.
rejectsReusedID :: ConcIO ()
rejectsReusedID = withSession $ \s -> do
  sendMessage (Request_wl_display_get_registry s.registry.wlid) s.display.wlid
  liftConc $ expectProtocolError s Err_invalid_object

-- | Run a client against a compositor. Both from this library.
withSession :: (Session -> Wayland Client ()) -> ConcIO ()
withSession test = withCompositor $ \kernel path compositor -> do
  ClientEnv client <- waylandConnect kernel table path
  clientLoopEnded <- newEmptyMVar
  callbacksDone <- watchCallbacks client
  bracket (forkClientLoop client clientLoopEnded) (\t -> killThread t >> close client.connectionSocket) $ \_ ->
    usingReaderT (ClientEnv client) $ do
      display <- getWlDisplay
      registry <- newObject display Request_wl_display_get_registry
      let session = Session{kernel, compositor, client = ClientEnv client, display, registry, clientLoopEnded, callbacksDone}
      roundtrip
      test session
  where
    forkClientLoop :: ConnectionEnv Client -> MVar SomeException -> ConcIO ThreadId
    forkClientLoop client ended = forkFinallyN "client-loop" (usingReaderT (ClientEnv client) $ clientLoop client.connectionSocket) (either (void . tryPutMVar ended) pure)

-- | Run a compositor from this library on a socket of its own, and something given the socket's path.
-- Waits for the compositor to drop its clients after, so they must disconnect.
-- A test that hangs fails, as dejafu sees every thread blocked.
withCompositor :: (Kernel -> FilePath -> ServerEnv -> ConcIO ()) -> ConcIO ()
withCompositor act = do
  kernel <- newKernel defaultKernelConfig
  let path = "/run/user/1000/wayland-0" :: FilePath
  bracket (waylandListen kernel table path) waylandServerClose $ \compositor -> do
    -- The client disconnecting ends the compositor's connection to it, which the compositor then drops.
    let compositorDropsClients = atomically $ readTVar compositor.clients >>= check . Map.null
    bracket (forkN "accept" $ listenForClients compositor) (\t -> compositorDropsClients >> killThread t) . const $ act kernel path compositor

-- }}}

-- vim: foldmethod=marker
