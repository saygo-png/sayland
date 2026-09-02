# Protocol Implementation

This doc explains how to implement a protocol (such as wayland-core, xdg-shell, etc.) in Sayland.

Required extensions:

```hs
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
```

Let's assume your protocol is [Fifo Protocol](https://wayland.app/protocols/fifo-v1) in  `protocols/fifo-v1.xml`.
First, define all enums:

```hs
import Protocol
import Sayland.Types
import Sayland.Utils
import Sayland.Protocols.Wayland -- you might not need to import this one, it depends on the protocol.

$(loadProtocolFileEnums False "protocols/fifo-v1.xml")
```

Next, define interface data types with a DefaultIO instance:

```hs
newtype Wp_fifo_manager_v1 = Wp_fifo_manager_v1 {wlid :: TObjectID Wp_fifo_manager_v1}
data Wp_fifo_v1 = Wp_fifo_v1 {wlid :: TObjectID Wp_fifo_v1, fifoSurface :: TObjectID Wl_surface}

instance DefaultIO Wp_fifo_manager_v1 where defM = pure $ Wp_fifo_manager_v1 0
instance DefaultIO Wp_fifo_v1 where defM = pure $ Wp_fifo_v1 0
```

Now, either implement `HasWlid` yourself or generate it with TH:

```hs
$(concat <$> mapM makeFieldsWithPrefix [''Wp_fifo_manager_v1, ''Wp_fifo_v1])
```

Load rest of the protocol (mostly `Event_*` and `Request_*` data types):

```hs
-- wlFormatter can be found in Sayland.Internal.Utils
$(loadProtocolFile wlFormatter False "protocols/fifo-v1.xml")
$(generateTables False wlFormatter "protocols/fifo-v1.xml")
```

Finally, implement `Interface'` to all your interface data types:

```hs

instance Interface' Wp_fifo_manager_v1 Client where
  type Request Wp_fifo_manager_v1 = Request_wp_fifo_manager_v1
  type Event Wp_fifo_manager_v1 = Event_wp_fifo_manager_v1
  runEvent _ _ = pass
  runRequest manager request@Request_wp_fifo_manager_v1_destroy = do
    sendMessage' request manager.wlid
    dropObject manager.wlid
  runRequest manager request@(Request_wp_fifo_manager_v1_get_fifo fifoId surfaceId) = do
    getInterface surfaceId >>= \case
      Just _ -> do
        fifoObj :: Wp_fifo_v1 <- defM
        void $ newObject fifoId fifoObj {fifoSurface = surfaceId}
        sendMessage' request manager.wlid
      Nothing -> error "non-existent surface provided to Request_wp_fifo_amanger_v1_get_fifo"

instance Interface' Wp_fifo_manager_v1 Server where
  type Request Wp_fifo_manager_v1 = Request_wp_fifo_manager_v1
  type Event Wp_fifo_manager_v1 = Event_wp_fifo_manager_v1
  runEvent _ _ = pass
  runRequest manager Request_wp_fifo_manager_v1_destroy = dropObject manager.wlid
  runRequest manager (Request_wp_fifo_manager_v1_get_fifo fifoId surfaceId) = do
    getInterface surfaceId >>= \case
      Just _ -> do
        fifoObj :: Wp_fifo_v1 <- defM
        void $ newObject fifoId fifoObj {fifoSurface = surfaceId}
      Nothing -> sendError manager.wlid 0 $ "surface `" <> show surfaceId <> "` does not exist"


instance Interface' Wp_fifo_v1 Client where
  type Request Wp_fifo_v1 = Request_wp_fifo_v1
  type Event Wp_fifo_v1 = Event_wp_fifo_v1
  runEvent _ _ = pass
  runRequest fifo request@Request_wp_fifo_v1_set_barrier = do
    getInterface fifo.fifoSurface >>= \case
      Just surface -> do
        atomicModifyIORef surface.pendingState $ \state' -> (state' {cuFifoBarrier = True},())
        sendMessage' request fifo.wlid
      Nothing -> error "the associated surface no longer exists"
  runRequest fifo request@Request_wp_fifo_v1_wait_barrier = do
    getInterface fifo.fifoSurface >>= \case
      Just surface -> do
        atomicModifyIORef surface.pendingState $ \state' -> (state' {cuFifoWaitBarrier = True},())
        sendMessage' request fifo.wlid
      Nothing -> error "the associated surface no longer exists"
  runRequest fifo request@Request_wp_fifo_v1_destroy = do
    sendMessage' request fifo.wlid
    dropObject fifo.wlid

instance Interface' Wp_fifo_v1 Server where
  type Request Wp_fifo_v1 = Request_wp_fifo_v1
  type Event Wp_fifo_v1 = Event_wp_fifo_v1
  runEvent _ _ = pass
  runRequest fifo Request_wp_fifo_v1_destroy = dropObject fifo.wlid
  runRequest fifo Request_wp_fifo_v1_set_barrier = do
    getInterface fifo.fifoSurface >>= \case
      Just surface -> atomicModifyIORef surface.pendingState $ \state' -> (state' {cuFifoBarrier = True},())
      Nothing -> sendError fifo.wlid 0 "the associated surface no longer exists"
  runRequest fifo Request_wp_fifo_v1_wait_barrier = do
    getInterface fifo.fifoSurface >>= \case
      Just surface -> atomicModifyIORef surface.pendingState $ \state' -> (state' {cuFifoWaitBarrier = True},())
      Nothing -> sendError fifo.wlid 0 "the associated surface no longer exists"
```

If you want only the client side of things, you can skip `Server` implementations, and vice versa.
