{-# LANGUAGE QuasiQuotes #-}

-- | Description : Race conditions, looked for by dejafu.
--
-- Each test is a scenario: a setup, run once with each thread running until it blocks, then a concurrent part whose
-- interleavings dejafu explores (up to two preemptions, with relaxed memory for IORefs). A test fails if any
-- interleaving breaks what the test checks, and the failure shows that interleaving's messages.
--
-- Some tests pass, and show that the code they cover is free of races dejafu can find. The others found races in
-- the library: their comments say what the race is.
module Races (tests) where

import Control.Exception (ErrorCall (..), IOException)
import Control.Monad.Catch (throwM, try)
import Data.ByteString qualified as BS
import Data.Map qualified as Map
import Data.Typeable (cast)
import Foreign (plusPtr)
import Harness
import RaceTest
import Sayland
import Sayland.Internal.Core (TObjectID (..), getConnectionEnv)
import Sayland.Kernel (KernelConfig (..), close, connect, defaultKernelConfig, fdInode, memfd, peekBytes, recv, setFdSize, socket)
import Sayland.Kernel qualified as Kernel
import Test.DejaFu (inspectTVar, registerInvariant, successful)
import Test.Tasty
import TestPrelude
import Utils

tests :: TestTree
tests =
  testGroup
    "Races"
    [ testGroup
        "Requests and objects"
        [ raceTest "roundtrip returns once the compositor replied" successful roundtripReturns
        , raceTest "client threads creating objects at once send their new IDs in order" successful clientIDsInOrder
        , raceTest "compositor threads creating objects at once send their new IDs in order" successful compositorIDsInOrder
        , raceTest "file descriptors sent by client threads at once arrive with their requests" successful fdsArriveWithRequests
        , raceTest "an event to an object the client destroyed does not leave its fd for the next event" successful noStaleFds
        , raceTest "no event handler runs for an object once its destroy request returned" successful noHandlerAfterDestroy
        , raceTest "two threads adding event handlers the way main's demos do keep both" successful eventHandlersKept
        , raceTest "the debug log shows requests in the order they were sent" successful logInWireOrder
        ]
    , testGroup
        "Connections"
        [ raceTest "a client that finished keeps its result when the compositor hangs up at the same time" successful finishedClientKeepsResult
        , raceTest "closing the compositor closes the connection of a client it is accepting" successful serverCloseClosesAccepted
        , raceTest "sending to a client that disconnects fails with Disconnected" successful sendToDisconnected
        , -- Too many steps to explore systematically, so it tries random interleavings, the same ones on each run.
          raceTestWith (randomInterleavings 1000 defaultRaceSettings) "full socket buffers in both directions do not deadlock the connection" successful fullBuffersDoNotDeadlock
        , raceTest "clients connecting at once get their own connections, which are dropped when they leave" successful clientsConnectAtOnce
        ]
    , testGroup
        "Surfaces and buffers"
        [ raceTest "two compositor threads completing a frame callback do not hang" successful frameCallbackCompletedTwice
        , raceTest "fifo updates are applied in order while the compositor clears the barrier" successful fifoUpdatesInOrder
        , raceTest "a destroyed subsurface does not come back when the compositor applies a queued update" successful destroyedSubsurfaceStaysGone
        , raceTest "the compositor can read a buffer while the client resizes its pool" successful readBufferWhileResizing
        ]
    , testGroup
        "Data devices"
        [ raceTest "two client threads setting the selection at once do not hang" successful setSelectionAtOnce
        , raceTest "a second drag started at once fails instead of hanging" successful startDragAtOnce
        , raceTest "a data source is destroyed once when cancelled while the client destroys it" successful cancelledSourceDestroyedOnce
        ]
    , testGroup
        "Outputs"
        [ raceTest "a client reading an output sees the properties of one wl_output.done" successful outputDoneIsAtomic
        ]
    , testGroup
        "Workspaces"
        [ raceTest "a workspace is in a group exactly when the group has the workspace" successful workspaceMembershipAgrees
        ]
    ]

table :: ProtocolTable
table = waylandTable <> xdg_shellTable <> fifo_v1Table <> ext_workspace_v1Table

-- | A client connected to a compositor on a kernel made with the config.
sessionWith :: KernelConfig -> Logs -> ConcIO Session
sessionWith config logs = do
  (kernel, compositor) <- listening (logged logs config) table
  connecting kernel compositor table

-- | A client connected to a compositor.
session :: Logs -> ConcIO Session
session = sessionWith defaultKernelConfig

-- | The compositor's object for an object the client made, after a `settle`.
compositorSide :: (Interface i) => Session -> i -> ConcIO i
compositorSide s obj = compositorObject s obj.wlid >>= maybe (failTest $ "the compositor has no " <> show (toObjectID obj.wlid)) pure

-- | The client's object for an object the compositor made, after a `settle`.
clientSide :: (Interface i) => Session -> i -> ConcIO i
clientSide s obj = asClient s (getInterface obj.wlid) >>= maybe (failTest $ "the client has no " <> show (toObjectID obj.wlid)) pure

-- | The connection of the client ended, with this exception.
connectionEnded :: Session -> ConcIO (Maybe SomeException)
connectionEnded s = tryReadMVar s.clientLoopEnded

-- | Fail if the client's connection ended.
connectionAlive :: Session -> ConcIO ()
connectionAlive s = connectionEnded s >>= traverse_ (\e -> failTest $ "the connection ended: " <> displayException e)

-- Requests and objects {{{

-- | `roundtrip` sends @wl_display.sync@ with `newObject`, which looks the new @wl_callback@ up after sending the request.
-- If the event loop handles the compositor's @done@ before that, it has already dropped the callback, and `newObject`
-- throws "handler did not register object". Any `newObject` whose object the peer can destroy right away has this race.
roundtripReturns :: Scenario ()
roundtripReturns = scenario session $ \s -> asClient s roundtrip

-- | `withNewObjectID` holds the ID counter while it sends, so the compositor never sees an ID skip ahead.
clientIDsInOrder :: Scenario ()
clientIDsInOrder = scenario setup $ \(s, wl_compositor) -> do
  let create = void . asClient s $ newObject wl_compositor Request_wl_compositor_create_surface
  concurrently create [("user-2", create)]
  settle s
  connectionAlive s
  where
    setup logs = do
      s <- session logs
      wl_compositor <- asClient s $ bindToInterface @Wl_compositor s.registry
      pure (s, wl_compositor)

-- | Like `clientIDsInOrder`, for objects the compositor makes, whose IDs the client checks.
compositorIDsInOrder :: Scenario ()
compositorIDsInOrder = scenario setup $ \(s, compositor, device) -> do
  let offer = void . runReaderT (newObject device Event_wl_data_device_data_offer) $ compositor
  concurrently offer [("compositor-2", offer)]
  settle s
  connectionAlive s
  where
    setup logs = do
      s <- session logs
      device <- asClient s $ do
        seat <- bindToInterface @Wl_seat s.registry
        manager <- bindToInterface @Wl_data_device_manager s.registry
        newObject manager $ \i -> Request_wl_data_device_manager_get_data_device i seat.wlid
      settle s
      compositor <- compositorEnv s
      (s,compositor,) <$> compositorSide s device

-- | Each request carries its fd in the same send, under the write lock, so the compositor matches them up whatever
-- the order the requests go in.
fdsArriveWithRequests :: Scenario ()
fdsArriveWithRequests = scenario setup $ \(s, shm, small, big) -> do
  let pool size fd = asClient s . newObject shm $ \i -> Request_wl_shm_create_pool i fd size
  concurrently (void $ pool 16 small) [("user-2", void $ pool 32 big)]
  settle s
  objects <- asCompositor s $ getConnectionEnv >>= readTVarIO . (.objects)
  for_ [p | SomeObject o <- Map.elems objects, Just (p :: Wl_shm_pool) <- [cast o]] $ \compositorPool -> do
    size <- readIORef compositorPool.size
    received <- fdInode s.kernel compositorPool.fd
    expected <- fdInode s.kernel $ if size == 16 then small else big
    received @?== expected
  where
    setup logs = do
      s <- session logs
      shm <- asClient s $ bindToInterface @Wl_shm s.registry
      small <- memfd s.kernel
      setFdSize s.kernel small 16
      big <- memfd s.kernel
      setFdSize s.kernel big 32
      pure (s, shm, small, big)

-- | The client drops an object when it sends its destroy request, and ignores the events still on their way to it.
-- But an ignored event's fds stay in the queue, and the next event that carries an fd takes the stale one.
-- libwayland knows how many fds each event of a destroyed object carries, and closes them.
--
-- Here the compositor sends a keymap to each of two keyboards while the client releases the first one. If the release
-- comes first, the second keyboard gets the first keymap's fd.
noStaleFds :: Scenario ()
noStaleFds = scenario setup $ \(s, compositor, keyboard1, keyboard2, (compositorKeyboard1, fd1), (compositorKeyboard2, fd2), keymaps) -> do
  let keymap keyboard fd = sendMsg keyboard $ Event_wl_keyboard_keymap Enum_wl_keyboard_keymap_format_xkb_v1 fd 1
      sendKeymaps = runReaderT (keymap compositorKeyboard1 fd1 >> keymap compositorKeyboard2 fd2) compositor
  concurrently (asClient s $ sendMsg keyboard1 Request_wl_keyboard_release) [("compositor", sendKeymaps)]
  received <- atomically $ readTVar keymaps >>= maybe retry pure . Map.lookup (toObjectID keyboard2.wlid)
  receivedFile <- fdInode s.kernel received
  sentFile <- fdInode s.kernel fd2
  assertTrue "the second keyboard got the first keymap's fd" $ receivedFile == sentFile
  where
    setup logs = do
      s <- session logs
      keymaps <- newTVarIO mempty
      let ClientEnv conn = s.client
      atomicModifyIORef' conn.eventHandlers $ \handlers ->
        let record oid = \case
              Event_wl_keyboard_keymap _ fd _ -> atomically . modifyTVar' keymaps $ Map.insert oid fd
              _ -> pass
         in (EventHandler record : handlers, ())
      (keyboard1, keyboard2) <- asClient s $ do
        seat <- bindToInterface @Wl_seat s.registry
        (,) <$> newObject seat Request_wl_seat_get_keyboard <*> newObject seat Request_wl_seat_get_keyboard
      settle s
      compositor <- compositorEnv s
      let withKeymap keyboard = do
            fd <- memfd s.kernel
            setFdSize s.kernel fd 1
            (,fd) <$> compositorSide s keyboard
      ck1 <- withKeymap keyboard1
      ck2 <- withKeymap keyboard2
      pure (s, compositor, keyboard1, keyboard2, ck1, ck2, keymaps)

-- | The event loop looks an object up, then runs its handlers. A destroy request in between does not stop them, so a
-- handler can run for an object whose destroy request already returned, after the client freed what it kept for it.
-- libwayland dispatches events in the thread that reads them, so with one thread this cannot happen.
noHandlerAfterDestroy :: Scenario ()
noHandlerAfterDestroy = scenario setup $ \(s, compositor, surface, compositorSurface, output, destroyed, lateEvents) -> do
  let destroy = do
        asClient s $ sendMsg surface Request_wl_surface_destroy
        atomicWriteIORef destroyed True
  concurrently destroy [("compositor", runReaderT (sendMsg compositorSurface $ Event_wl_surface_enter output.wlid) compositor)]
  settle s
  readIORef lateEvents >>=@?== (0 :: Int)
  where
    setup logs = do
      s <- session logs
      destroyed <- newIORef False
      lateEvents <- newIORef 0
      (surface, output) <- asClient s $ do
        wl_compositor <- bindToInterface @Wl_compositor s.registry
        (,) <$> newObject wl_compositor Request_wl_compositor_create_surface <*> bindToInterface @Wl_output s.registry
      let ClientEnv conn = s.client
          lateEvent oid (_ :: Event_wl_surface) =
            when (oid == toObjectID surface.wlid) $ readIORef destroyed >>= \yes -> when yes $ atomicModifyIORef' lateEvents (\n -> (n + 1, ()))
      atomicModifyIORef' conn.eventHandlers $ \handlers -> (EventHandler lateEvent : handlers, ())
      settle s
      compositor <- compositorEnv s
      compositorSurface <- compositorSide s surface
      pure (s, compositor, surface, compositorSurface, output, destroyed, lateEvents)

-- | Main has no function to add an event handler. Its demos read the list, add one, and write the list back, with
-- 'modifyIORef', so two threads doing so at once can lose one of the handlers.
eventHandlersKept :: Scenario ()
eventHandlersKept = scenario session $ \s -> do
  let ClientEnv conn = s.client
      before = length <$> readIORef conn.eventHandlers
      add = modifyIORef conn.eventHandlers (EventHandler (\_ (_ :: Event_wl_display) -> pass) :)
  count <- before
  concurrently add [("user-2", add)]
  before >>=@?== count + 2

-- | `sendMessage` logs a message before it takes the write lock to send it. So with @WAYLAND_DEBUG@ on, two threads
-- sending at once can log their messages in the opposite order to the one they go over the wire in.
logInWireOrder :: Scenario ()
logInWireOrder = scenario setup $ \(s, client, sentLog, received, surface1, surface2) -> do
  let scale surface = usingReaderT client . sendMsg surface $ Request_wl_surface_set_buffer_scale 2
  concurrently (scale surface1) [("user-2", scale surface2)]
  settle s
  inLog <- reverse <$> readTVarIO sentLog
  wire <- reverse <$> readTVarIO received
  -- A logged message reads like @wl_surface#5.set_buffer_scale { scale: 2 }@.
  let objectIn line = takeWhile (/= '.') . drop 1 $ dropWhile (/= '#') line
  (objectIn <$> inLog) @?== (show . fromAnyObjectID <$> wire)
  where
    setup logs = do
      s <- session logs
      sent <- newTVarIO []
      received <- newTVarIO []
      -- The client's log, as WAYLAND_DEBUG=client shows it.
      let ClientEnv conn = s.client
          logger = Logger{minLevel = LogDebug, write = \_ _ _ -> pass, writeMessage = \_ _ text -> atomically $ modifyTVar' sent (text :)}
          recordScale oid = \case
            Request_wl_surface_set_buffer_scale _ -> atomically $ modifyTVar' received (oid :)
            _ -> pass
      atomicModifyIORef' s.compositor.eventHandlers $ \handlers -> (EventHandler recordScale : handlers, ())
      (surface1, surface2) <- asClient s $ do
        wl_compositor <- bindToInterface @Wl_compositor s.registry
        (,) <$> newObject wl_compositor Request_wl_compositor_create_surface <*> newObject wl_compositor Request_wl_compositor_create_surface
      settle s
      pure (s, ClientEnv conn{logger}, sent, received, surface1, surface2)

-- }}}

-- Connections {{{

-- | `withEventLoop` hands the exception that ends the event loop to the client with 'throwTo'. When the compositor
-- hangs up as the client finishes, the exception can arrive after the client's last action, before `withEventLoop`
-- masks exceptions to kill the loop. The client's result is lost, and `withEventLoop` throws `Disconnected` though the
-- client did all it had to. Racing the client's result against the loop's end (like @async@'s @race@) would keep it.
finishedClientKeepsResult :: Scenario ()
finishedClientKeepsResult = scenario setup $ \(client, serverSocket, finished) -> do
  let run = do
        result <- try . usingReaderT client . withEventLoop $ (42 :: Int) <$ atomicWriteIORef finished True
        done <- readIORef finished
        case result of
          Left (e :: SomeException) | done -> failTest $ "the client did all it had to, then withEventLoop threw: " <> displayException e
          _ -> pass
  concurrently run [("compositor", close serverSocket)]
  where
    setup logs = do
      (kernel, compositor) <- listening (logged logs defaultKernelConfig) table
      client <- waylandSetup kernel table
      serverSocket <- atomically $ readTVar compositor.clients >>= maybe retry (pure . (.connectionSocket)) . listToMaybe . Map.elems
      finished <- newIORef False
      pure (client, serverSocket, finished)

-- | `waylandServerClose` closes the connections in the client map, but a client the accept loop accepted and did not add
-- yet is missed: its connection stays open, served by a thread of the closed compositor, and the client never hears
-- the compositor went away.
serverCloseClosesAccepted :: Scenario ()
serverCloseClosesAccepted = scenario setup $ \(kernel, compositor) -> do
  let path = compositor.socketPath
      untilClosed sock = recv sock 8 >>= \(bytes, _) -> unless (BS.null bytes) (untilClosed sock)
      client = do
        sock <- socket kernel
        connected <- try $ connect sock path
        case connected of
          Left (_ :: IOException) -> pass -- The compositor closed first.
          Right () -> untilClosed sock
  concurrently (waylandServerClose compositor) [("client", client)]
  where
    setup logs = listening (logged logs defaultKernelConfig) table

-- | When a client disconnects, the compositor's thread for it closes the socket. A compositor thread sending to the
-- client in the meantime gets `Disconnected` while the socket is open, but an @EBADF@ 'IOError' once it is closed, which
-- a compositor that handles disconnects by catching `Disconnected` does not expect.
sendToDisconnected :: Scenario ()
sendToDisconnected = scenario setup $ \(s, compositor, compositorRegistry) -> do
  let ClientEnv conn = s.client
      send = do
        result <- try . usingReaderT compositor . sendMsg compositorRegistry $ Event_wl_registry_global_remove 7
        case result of
          Left (e :: SomeException) | Just Disconnected <- fromException e -> pass
          Left e -> failTest $ "sending to a client that disconnected threw: " <> displayException e
          Right () -> pass
  concurrently (close conn.connectionSocket) [("compositor", send >> send)]
  where
    setup logs = do
      s <- session logs
      compositor <- compositorEnv s
      (s,compositor,) <$> compositorSide s s.registry

-- | The event loop sends some requests itself, like @xdg_wm_base.pong@. Sending blocks while the socket's buffer is full,
-- so the loop can block on a pong while a client thread holds the write lock, blocked on a request the compositor does
-- not read, as the compositor's thread is blocked sending an event the client does not read. Each side waits on the
-- other. libwayland never blocks on a full buffer: it queues what it sends and flushes later.
--
-- Here the buffers hold one message, so a few messages fill them. A real kernel's hold hundreds of kilobytes, which
-- takes a flood of events and requests at once to fill.
fullBuffersDoNotDeadlock :: Scenario ()
fullBuffersDoNotDeadlock = scenario setup $ \(s, compositor, compositorWmBase, pongs) -> do
  let sync = asClient s . withNewObjectID $ \i -> sendMsg s.display (Request_wl_display_sync . TObjectID $ FromClient i)
      ping = runReaderT (sendMsg compositorWmBase $ Event_xdg_wm_base_ping 1) compositor
  concurrently (replicateM_ 3 sync) [("compositor", ping)]
  atomically $ readTVar pongs >>= check . (>= 1)
  atomically $ readTVar s.callbacksDone >>= check . (>= 4) . length
  where
    setup logs = do
      s <- sessionWith defaultKernelConfig{socketBuffer = Just 12} logs
      pongs <- newTVarIO (0 :: Int)
      atomicModifyIORef' s.compositor.eventHandlers $ \handlers ->
        let pong _ = \case
              Request_xdg_wm_base_pong _ -> atomically $ modifyTVar' pongs (+ 1)
              _ -> pass
         in (EventHandler pong : handlers, ())
      wmBase <- asClient s $ bindToInterface @Xdg_wm_base s.registry
      settle s
      compositor <- compositorEnv s
      (s,compositor,,pongs) <$> compositorSide s wmBase

-- | The client serial is taken in a transaction, so each client gets its own entry in the client map.
clientsConnectAtOnce :: Scenario ()
clientsConnectAtOnce = scenario setup $ \(kernel, compositor) -> do
  seen <- newTVarIO (0 :: Int)
  let client = do
        env <- waylandSetup kernel table
        -- Both clients are in the map at once, so they got different serials. Neither leaves before both saw that.
        atomically $ readTVar compositor.clients >>= check . (== 2) . Map.size
        atomically $ modifyTVar' seen (+ 1)
        atomically $ readTVar seen >>= check . (== 2)
        waylandClose env
  concurrently client [("client-2", client)]
  atomically $ readTVar compositor.clients >>= check . Map.null
  where
    setup logs = listening (logged logs defaultKernelConfig) table

-- }}}

-- Surfaces and buffers {{{

-- | The compositor completes a frame callback with @wl_callback.done@, whose handler puts into the callback's 'MVar'.
-- A compositor can only check the callback is still there before it sends, so two compositor threads completing
-- the callbacks of a surface (say, the frame clocks of two outputs it is on) can both send @done@, and the second
-- blocks forever on the full 'MVar'.
frameCallbackCompletedTwice :: Scenario ()
frameCallbackCompletedTwice = scenario setup $ \(compositor, compositorSurface) -> do
  let complete = flip runReaderT compositor $ do
        callbacks <- (.sCallbacks) <$> readTVarIO compositorSurface.state
        for_ callbacks $ getInterface >=> traverse_ (\callback -> sendMsg callback $ Event_wl_callback_done 0)
  concurrently complete [("compositor-2", complete)]
  where
    setup logs = do
      s <- session logs
      surface <- asClient s $ do
        wl_compositor <- bindToInterface @Wl_compositor s.registry
        surface <- newObject wl_compositor Request_wl_compositor_create_surface
        void $ newObject surface Request_wl_surface_frame
        sendMsg surface Request_wl_surface_commit
        pure surface
      settle s
      (,) <$> compositorEnv s <*> compositorSide s surface

-- | Commits and barrier clears both flush the surface's queue in a transaction, so each update is applied once, in
-- the order the client committed them, whichever comes first.
fifoUpdatesInOrder :: Scenario ()
fifoUpdatesInOrder = scenario setup $ \(s, compositor, surface, fifo, compositorSurface) -> do
  let commits = asClient s . for_ [2, 3] $ \scale -> do
        sendMsg fifo Request_wp_fifo_v1_wait_barrier
        sendMsg fifo Request_wp_fifo_v1_set_barrier
        sendMsg surface $ Request_wl_surface_set_buffer_scale scale
        sendMsg surface Request_wl_surface_commit
      present = do
        latest <- atomically $ do
          scale <- (.sBufferScale) <$> readTVar compositorSurface.state
          queued <- readTVar compositorSurface.queue
          scale <$ check (scale == 3 || not (null queued))
        unless (latest == 3) $ do
          env <- runReaderT getConnectionEnv compositor
          atomically $ clearFifoBarrier env compositorSurface
          present
  concurrently commits [("compositor", present)]
  where
    setup logs = do
      s <- session logs
      (surface, fifo) <- asClient s $ do
        wl_compositor <- bindToInterface @Wl_compositor s.registry
        surface <- newObject wl_compositor Request_wl_compositor_create_surface
        manager <- bindToInterface @Wp_fifo_manager_v1 s.registry
        (surface,) <$> newObject manager (\i -> Request_wp_fifo_manager_v1_get_fifo i surface.wlid)
      settle s
      compositor <- compositorEnv s
      compositorSurface <- compositorSide s surface
      -- Applied updates only ever raise the scale: the queue never applies one out of order.
      registerInvariant $ do
        current <- inspectTVar compositorSurface.state
        queued <- inspectTVar compositorSurface.queue
        let scales = [scale | DesyncContentUpdate cu <- toList queued, Just scale <- [cu.cuBufferScale]]
        when (any ((<= current.sBufferScale) . fromIntegral) scales) . throwM $ ErrorCall "an update was applied before one committed earlier"
      pure (s, compositor, surface, fifo, compositorSurface)

-- | Destroying a subsurface takes it out of its parent's stack and out of the parent's queued updates in one
-- transaction, so a queued update the compositor applies at the same time does not bring it back.
destroyedSubsurfaceStaysGone :: Scenario ()
destroyedSubsurfaceStaysGone = scenario setup $ \(s, compositor, subsurface, child, compositorParent) -> do
  let present = do
        env <- runReaderT getConnectionEnv compositor
        atomically $ clearFifoBarrier env compositorParent
  concurrently (asClient s $ sendMsg subsurface Request_wl_subsurface_destroy) [("compositor", present)]
  settle s
  current <- (.sSubsurfaces) <$> readTVarIO compositorParent.state
  queued <- readTVarIO compositorParent.queue
  let stacks = current : [stack | DesyncContentUpdate cu <- toList queued, Just stack <- [cu.cuSubsurfaces]]
  assertTrue "the destroyed subsurface is in its parent's stack" $ all (\stack -> child.wlid `notElem` (stack.above <> stack.below)) stacks
  where
    setup logs = do
      s <- session logs
      (subsurface, parent, child, fifo) <- asClient s $ do
        wl_compositor <- bindToInterface @Wl_compositor s.registry
        subcompositor <- bindToInterface @Wl_subcompositor s.registry
        manager <- bindToInterface @Wp_fifo_manager_v1 s.registry
        parent <- newObject wl_compositor Request_wl_compositor_create_surface
        child <- newObject wl_compositor Request_wl_compositor_create_surface
        subsurface <- newObject subcompositor $ \i -> Request_wl_subcompositor_get_subsurface i child.wlid parent.wlid
        fifo <- newObject manager $ \i -> Request_wp_fifo_manager_v1_get_fifo i parent.wlid
        -- Raise the parent's barrier, so its next update waits.
        sendMsg fifo Request_wp_fifo_v1_set_barrier
        sendMsg parent Request_wl_surface_commit
        pure (subsurface, parent, child, fifo)
      settle s
      compositor <- compositorEnv s
      compositorParent <- compositorSide s parent
      -- The next update puts the child above its parent.
      atomicModifyIORef' compositorParent.pendingState $ \cu -> (cu{cuSubsurfaces = Just SubsurfaceStack{above = pure child.wlid, below = mempty}}, ())
      asClient s $ do
        sendMsg fifo Request_wp_fifo_v1_wait_barrier
        sendMsg parent Request_wl_surface_commit
      settle s
      queued <- readTVarIO compositorParent.queue
      assertTrue "the update does not wait" $ length queued == 1
      pure (s, compositor, subsurface, child, compositorParent)

-- | A compositor reads a shm buffer through its pool's mapping, from the thread that renders. @wl_shm_pool.resize@
-- unmaps the pool on the client's thread, at once, so the renderer can read memory that is no longer mapped.
-- libwayland defers a resize while the pool is being read (@wl_shm_buffer_begin_access@).
readBufferWhileResizing :: Scenario ()
readBufferWhileResizing = scenario setup $ \(s, pool, shmBuffer) -> do
  let render = do
        mapped <- readIORef shmBuffer.pool.ptr
        void . peekBytes s.kernel (mapped `plusPtr` fromIntegral shmBuffer.offset) $ fromIntegral (shmBuffer.stride * 2)
  concurrently (asClient s . sendMsg pool $ Request_wl_shm_pool_resize 32) [("renderer", render)]
  settle s
  atomically (Kernel.faults s.kernel) >>=@?== []
  where
    setup logs = do
      s <- session logs
      fd <- memfd s.kernel
      setFdSize s.kernel fd 16
      (pool, buffer) <- asClient s $ do
        shm <- bindToInterface @Wl_shm s.registry
        pool <- newObject shm $ \i -> Request_wl_shm_create_pool i fd 16
        (pool,) <$> newObject pool (\i -> Request_wl_shm_pool_create_buffer i 0 2 2 8 Enum_wl_shm_format_argb8888)
      settle s
      Wl_buffer{buffer = Buffer backend} <- compositorSide s buffer
      shmBuffer :: ShmBuffer <- maybe (failTest "the buffer is not in shared memory") pure $ cast backend
      pure (s, pool, shmBuffer)

-- }}}

-- Data devices {{{

-- | A data device and two data sources.
withDataDevice :: Logs -> ConcIO (Session, Wl_data_device, Wl_data_source, Wl_data_source, Wl_surface)
withDataDevice logs = do
  s <- session logs
  objects <- asClient s $ do
    seat <- bindToInterface @Wl_seat s.registry
    manager <- bindToInterface @Wl_data_device_manager s.registry
    wl_compositor <- bindToInterface @Wl_compositor s.registry
    device <- newObject manager $ \i -> Request_wl_data_device_manager_get_data_device i seat.wlid
    source1 <- newObject manager Request_wl_data_device_manager_create_data_source
    source2 <- newObject manager Request_wl_data_device_manager_create_data_source
    surface <- newObject wl_compositor Request_wl_compositor_create_surface
    pure (s, device, source1, source2, surface)
  settle s
  pure objects

-- | @set_selection@ empties the selection 'MVar', then fills it. Two threads can both empty it before either fills it,
-- and the second to fill it blocks forever.
setSelectionAtOnce :: Scenario ()
setSelectionAtOnce = scenario withDataDevice $ \(s, device, source1, source2, _) -> do
  let select source serial = asClient s . sendMsg device $ Request_wl_data_device_set_selection (Just source.wlid) serial
  concurrently (select source1 1) [("user-2", select source2 2)]

-- | @start_drag@ checks the drag 'MVar' is empty, to throw if a drag is on, then fills it. Two threads can both find it
-- empty, and the second to fill it blocks forever instead of throwing.
startDragAtOnce :: Scenario ()
startDragAtOnce = scenario withDataDevice $ \(s, device, source1, source2, surface) -> do
  let drag source serial = do
        started <- try . asClient s . sendMsg device $ Request_wl_data_device_start_drag (Just source.wlid) surface.wlid Nothing serial
        case started of
          Left (ErrorCall _) -> pass -- The other drag started first.
          Right () -> pass
  concurrently (drag source1 1) [("user-2", drag source2 2)]

-- | On @wl_data_source.cancelled@ the event loop destroys the source. A client thread destroying it at the same time
-- sends a second destroy for an object the compositor already dropped, which libwayland's compositors end the
-- connection for (sayland's ignores it, with a warning).
cancelledSourceDestroyedOnce :: Scenario ()
cancelledSourceDestroyedOnce = scenario setup $ \(s, compositor, source, compositorSource, warnings) -> do
  concurrently
    (asClient s $ sendMsg source Request_wl_data_source_destroy)
    [("compositor", runReaderT (sendMsg compositorSource Event_wl_data_source_cancelled) compositor)]
  -- The first settle waits for the event loop to handle the cancel, which comes before its done. The second for the
  -- compositor to handle the destroy the event loop sent then.
  settle s
  settle s
  readTVarIO warnings >>=@?== []
  where
    setup logs = do
      warnings <- newTVarIO []
      let recordWarnings logger =
            logger
              { minLevel = min LogWarn logger.minLevel
              , write = \level client text -> do
                  when (level >= LogWarn) . atomically $ modifyTVar' warnings (text :)
                  when (level >= logger.minLevel) $ logger.write level client text
              }
      (kernel, compositor) <- listeningWith (\c -> c{serverLogger = recordWarnings c.serverLogger}) (logged logs defaultKernelConfig) table
      s <- connecting kernel compositor table
      source <- asClient s $ do
        manager <- bindToInterface @Wl_data_device_manager s.registry
        newObject manager Request_wl_data_device_manager_create_data_source
      settle s
      env <- compositorEnv s
      (s,env,source,,warnings) <$> compositorSide s source

-- }}}

-- Outputs {{{

-- | @wl_output.done@ applies the properties sent since the last one, atomically, as the protocol says. But it writes
-- them to an 'IORef' each, so a client thread reading two of them can see one before and one after.
outputDoneIsAtomic :: Scenario ()
outputDoneIsAtomic = scenario setup $ \(compositor, output, compositorOutput) -> do
  let update = usingReaderT compositor $ do
        sendMsg compositorOutput $ Event_wl_output_geometry 0 0 0 0 Enum_wl_output_subpixel_unknown [wl|second|] [wl|model|] Enum_wl_output_transform_normal
        sendMsg compositorOutput $ Event_wl_output_mode (Enum_wl_output_mode True False) 2 2 60000
        sendMsg compositorOutput Event_wl_output_done
      look = do
        geometry <- readIORef output.outputGeometry
        mode <- readIORef output.outputMode
        assertTrue "the output's make and mode come from different wl_output.done" $ (geometry.make == [wl|second|]) == (mode.modeWidth == 2)
  concurrently look [("compositor", update)]
  where
    setup logs = do
      s <- session logs
      output <- asClient s $ bindToInterface @Wl_output s.registry
      settle s
      compositor <- compositorEnv s
      compositorOutput <- compositorSide s output
      usingReaderT compositor $ do
        sendMsg compositorOutput $ Event_wl_output_geometry 0 0 0 0 Enum_wl_output_subpixel_unknown [wl|first|] [wl|model|] Enum_wl_output_transform_normal
        sendMsg compositorOutput $ Event_wl_output_mode (Enum_wl_output_mode True False) 1 1 60000
        sendMsg compositorOutput Event_wl_output_done
      settle s
      pure (compositor, output, compositorOutput)

-- }}}

-- Workspaces {{{

-- | Group membership is changed in transactions that check the group and the workspace are alive, so it agrees in both
-- directions after every step, while the client destroys the group as the compositor moves the workspace.
workspaceMembershipAgrees :: Scenario ()
workspaceMembershipAgrees = scenario setup $ \(s, compositor, workspaceGroup, compositorGroup, compositorWorkspace) -> do
  let move = usingReaderT compositor $ do
        sendMsg compositorGroup $ Event_ext_workspace_group_handle_v1_workspace_leave compositorWorkspace.wlid
        sendMsg compositorGroup $ Event_ext_workspace_group_handle_v1_workspace_enter compositorWorkspace.wlid
  concurrently (asClient s $ sendMsg workspaceGroup Request_ext_workspace_group_handle_v1_destroy) [("compositor", move)]
  settle s
  where
    setup logs = do
      s <- session logs
      manager <- asClient s $ bindToInterface @Ext_workspace_manager_v1 s.registry
      settle s
      compositor <- compositorEnv s
      compositorManager <- compositorSide s manager
      (compositorGroup, compositorWorkspace) <- usingReaderT compositor $ do
        workspaceGroup <- newObject compositorManager Event_ext_workspace_manager_v1_workspace_group
        workspace <- newObject compositorManager Event_ext_workspace_manager_v1_workspace
        sendMsg workspaceGroup $ Event_ext_workspace_group_handle_v1_workspace_enter workspace.wlid
        pure (workspaceGroup, workspace)
      settle s
      workspaceGroup <- clientSide s compositorGroup
      workspace <- clientSide s compositorWorkspace
      registerInvariant $ do
        workspaces <- inspectTVar workspaceGroup.groupWorkspaces
        groups <- inspectTVar workspace.workspaceGroups
        let inGroup = any (\w -> w.wlid == workspace.wlid) workspaces
            hasGroup = any (\g -> g.wlid == workspaceGroup.wlid) groups
        when (inGroup /= hasGroup) . throwM $ ErrorCall "the group and the workspace disagree on whether it is in the group"
      pure (s, compositor, workspaceGroup, compositorGroup, compositorWorkspace)

-- }}}

-- vim: foldmethod=marker
