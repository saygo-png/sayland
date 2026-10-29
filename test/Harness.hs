-- | Description : A compositor and its client from this library, on a simulated kernel.
module Harness (module Harness) where

import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Traversable (for)
import Sayland
import Sayland.Internal.Core (TObjectID (..))
import Sayland.Kernel (Kernel, KernelConfig, newKernel)
import TestPrelude
import Utils

-- | A client connected to a compositor of its own, with the registry's globals already received.
data Session = Session
  { kernel :: Kernel
  , compositor :: ServerEnv
  , client :: WaylandEnv Client
  , display :: Wl_display
  , registry :: Wl_registry
  , clientLoopEnded :: MVar SomeException
  -- ^ Filled when the client's event loop ends, which it only does with an exception.
  , callbacksDone :: TVar (Set AnyObjectID)
  -- ^ The @wl_callback@s the client got @done@ for. See `settle`.
  }

-- | A kernel with a compositor listening on it, in @XDG_RUNTIME_DIR@, and accepting clients in a thread named @accept@.
listening :: KernelConfig -> ProtocolTable -> ConcIO (Kernel, ServerEnv)
listening = listeningWith id

-- | `listening`, with a change to the compositor before it accepts clients.
listeningWith :: (ServerEnv -> ServerEnv) -> KernelConfig -> ProtocolTable -> ConcIO (Kernel, ServerEnv)
listeningWith change config protocols = do
  kernel <- newKernel config
  compositor <- change <$> waylandServerSetup kernel protocols
  void . forkN "accept" $ listenForClients compositor
  pure (kernel, compositor)

-- | Connect a client to the compositor, as `waylandSetup` finds it, and run its event loop in a thread named
-- @client-loop@. Gets the registry and waits for its globals.
connecting :: Kernel -> ServerEnv -> ProtocolTable -> ConcIO Session
connecting kernel compositor protocols = do
  client@(ClientEnv conn) <- waylandSetup kernel protocols
  clientLoopEnded <- newEmptyMVar
  callbacksDone <- watchCallbacks conn
  void . forkFinallyN "client-loop" (runReaderT (clientLoop conn.connectionSocket) client) $ either (void . tryPutMVar clientLoopEnded) pure
  (display, registry) <- flip runReaderT client $ do
    display <- getWlDisplay
    registry <- newObject display Request_wl_display_get_registry
    roundtrip
    pure (display, registry)
  pure Session{kernel, compositor, client, display, registry, clientLoopEnded, callbacksDone}

-- | Record the @wl_callback@s the client gets @done@ for.
watchCallbacks :: ConnectionEnv Client -> ConcIO (TVar (Set AnyObjectID))
watchCallbacks conn = do
  done <- newTVarIO mempty
  atomicModifyIORef' conn.eventHandlers $ \handlers ->
    (EventHandler (\oid (Event_wl_callback_done _) -> atomically . modifyTVar' done $ Set.insert oid) : handlers, ())
  pure done

-- | Wait until the compositor handled every request sent before, and the client every event it sent while doing so.
-- `roundtrip` does that too, but it has a race of its own (see the roundtrip test), so this waits for the
-- @wl_callback.done@ with an event handler instead of the callback object.
settle :: Session -> ConcIO ()
settle s = do
  callback <- asClient s . withNewObjectID $ \i ->
    FromClient i <$ sendMsg s.display (Request_wl_display_sync . TObjectID $ FromClient i)
  atomically $ readTVar s.callbacksDone >>= check . Set.member callback

-- | Run something as the client.
asClient :: Session -> Wayland Client a -> ConcIO a
asClient s act = runReaderT act s.client

-- | Run something as the compositor, on its connection to its only client.
asCompositor :: Session -> Wayland Server a -> ConcIO a
asCompositor s act =
  readTVarIO s.compositor.clients >>= \clients -> case Map.toList clients of
    [(serial, client)] -> runReaderT act (ClientServerEnv s.compositor client serial)
    _ -> failTest $ "the compositor has " <> show (Map.size clients) <> " clients, not 1"

-- | Get an object from the compositor from its only client.
compositorObject :: (Typeable i) => Session -> TObjectID i -> ConcIO (Maybe i)
compositorObject s = asCompositor s . getInterface

-- | The compositor's object for a surface the client made.
onCompositor :: Session -> Wl_surface -> Wayland Client Wl_surface
onCompositor s surface = liftConc $ compositorObject s surface.wlid >>= maybe (failTest "the compositor has no wl_surface") pure

-- | The client's connection ends with a @wl_display.error@ about the display, with this code.
expectProtocolError :: Session -> Enum_wl_display_error -> ConcIO ()
expectProtocolError s code = do
  ended <- takeMVar s.clientLoopEnded
  case fromException ended of
    Just (e :: ProtocolError) -> do
      e.object @?== Just (toObjectID s.display.wlid)
      e.code @?== errorCode code
    Nothing -> failTest $ "the connection ended with something else: " <> displayException ended

-- | The compositor's connection to its only client, to run things as the compositor with.
compositorEnv :: Session -> ConcIO (WaylandEnv Server)
compositorEnv s =
  readTVarIO s.compositor.clients >>= \clients -> case Map.toList clients of
    [(serial, client)] -> pure $ ClientServerEnv s.compositor client serial
    _ -> failTest $ "the compositor has " <> show (Map.size clients) <> " clients, not 1"

-- | Run an action in this thread and others in threads of the given names, all at once, and wait for all of them.
-- Rethrows what any of the others threw, so it fails the test as if this thread threw it.
concurrently :: ConcIO () -> [(String, ConcIO ())] -> ConcIO ()
concurrently here others = do
  ended <- for others $ \(name, act) -> do
    done <- newEmptyMVar
    void . forkFinallyN name act $ putMVar done
    pure done
  here
  for_ ended $ takeMVar >=> either throwIO pure
