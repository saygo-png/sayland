# Protocol Implementation

This doc explains how to implement a protocol (such as wayland-core, xdg-shell, etc.) in Sayland.

Let's assume your protocol is [Fifo Protocol](https://wayland.app/protocols/fifo-v1) in `xml-protocols/fifo-v1.xml`.

First we create a public api re-export module in `lib/Sayland/Protocols/Fifo.hs` which can be as simple as:

```hs
-- | Description: Implementation of @wayland-protocols/staging/fifo-v1@.
module Sayland.Protocols.Fifo (module Sayland.Internal.Protocols.Fifo) where

import Sayland.Internal.Protocols.Fifo
```

You can add `hiding` here to hide internal functions specific to a protocol.
For most protocol implementations you want to export the whole thing.

Second we create an internal module in `lib/Sayland/Internal/Protocols/Fifo.hs`.
This file contains the actual implementation. Having 2 modules allows us to control public vs internal APIs.

Add some necessary language extensions and a haddock pragma which makes it so docs are placed on the public module:

```hs
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK not-home #-}
```

Import necessary Sayland modules and generate enums:

```hs
import Sayland.Internal.Codegen
import Sayland.Internal.Core
import Sayland.Internal.Object
import Sayland.Internal.Protocols.Wayland -- you might not need to import this one, it depends on the protocol.

$(loadProtocolFileEnums False "xml-protocols/fifo-v1.xml")
```

Next, define interface data types following the given protocol.
Here `Wp_fifo_manager_v1` does not have any fields listed, but every interface carries a wlid.
It's also a Wayland global, because it has no creation requests. It's what a client binds to to access other interfaces defined in the protocol.

The type `Wp_fifo_v1` is derived from the signature for its creation request `wp_fifo_manager_v1::get_fifo` defined in the xml.
A nice way to view these signatures is using [wayland.app](https://wayland.app/protocols/fifo-v1#wp_fifo_manager_v1:request:get_fifo).
It formats them like this: `get_fifo(id: new_id<wp_fifo_v1>, surface: object<wl_surface>)`
We translate `new_id<wp_fifo_v1>` to `TObjectID Wp_fifo_v1`, and `surface: object<wl_surface>` to `TObjectID Wl_surface`.

```hs
newtype Wp_fifo_manager_v1 = Wp_fifo_manager_v1 {wlid :: TObjectID Wp_fifo_manager_v1}
data Wp_fifo_v1 = Wp_fifo_v1 {wlid :: TObjectID Wp_fifo_v1, fifoSurface :: TObjectID Wl_surface}
```

After defining interfaces, we load the rest of the protocol (mostly `Event_*` and `Request_*` data types):

```hs
-- wlFormatter can be found in Sayland.Internal.Codegen
$(loadProtocolFile wlFormatter False "protocols/fifo-v1.xml")
```

Implement an `Object` instance for all your interface data types:
You can find more details on how to do this in the [implementing objects guide](./implementing_objects.md)

```hs
instance Object Wp_fifo_manager_v1 where
  onRequest manager msg@Request_wp_fifo_manager_v1_destroy = do
    forwardMessage manager msg
    dropObject manager.wlid
  onRequest manager msg@(Request_wp_fifo_manager_v1_get_fifo fifoId surfaceId) = do
    getInterface surfaceId >>= \case
      Just _ -> do
        registerObject Wp_fifo_v1{wlid = fifoId, fifoSurface = surfaceId}
        forwardMessage manager msg
      Nothing -> protocolErrorG Err_invalid_object $ "wp_fifo_manager_v1.get_fifo: surface `" <> fromString (show surfaceId) <> "` does not exist"

  onEvent _ = \case {}

instance Object Wp_fifo_v1 where
  onRequest fifo msg@Request_wp_fifo_v1_set_barrier = do
    getInterface fifo.fifoSurface >>= \case
      Just surface -> do
        atomicModifyIORef' surface.pendingState $ \state -> (state{cuFifoBarrier = True}, ())
        forwardMessage fifo msg
      Nothing -> protocolError fifo Enum_wp_fifo_v1_error_surface_destroyed "set_barrier: the associated surface no longer exists"
  onRequest fifo msg@Request_wp_fifo_v1_wait_barrier = do
    getInterface fifo.fifoSurface >>= \case
      Just surface -> do
        atomicModifyIORef' surface.pendingState $ \state -> (state{cuFifoWaitBarrier = True}, ())
        forwardMessage fifo msg
      Nothing -> protocolError fifo Enum_wp_fifo_v1_error_surface_destroyed "wait_barrier: the associated surface no longer exists"
  onRequest fifo msg@Request_wp_fifo_v1_destroy = do
    forwardMessage fifo msg
    dropObject fifo.wlid

  onEvent _ = \case {}
```

And finally generate tables:

```hs
$(generateTables False wlFormatter "protocols/fifo-v1.xml")
```

The ordering here matters for what the template haskell splices can see, make sure it is correct.
