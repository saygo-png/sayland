{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | Description: Internals of Sayland.Protocols.Fifo
module Sayland.Internal.Protocols.Fifo (module Sayland.Internal.Protocols.Fifo) where

import Data.String (fromString)
import Sayland.Internal.Codegen
import Sayland.Internal.Core
import Sayland.Internal.Object
import Sayland.Internal.Prelude
import Sayland.Internal.Protocols.Wayland

$(loadProtocolFileEnums False "xml-protocols/fifo-v1.xml")

-- Interfaces {{{
newtype Wp_fifo_manager_v1 = Wp_fifo_manager_v1 {wlid :: TObjectID Wp_fifo_manager_v1}

data Wp_fifo_v1 = Wp_fifo_v1 {wlid :: TObjectID Wp_fifo_v1, fifoSurface :: TObjectID Wl_surface}

-- }}}

$(loadProtocolFile wlFormatter False "xml-protocols/fifo-v1.xml")

-- Implementations {{{
-- Wp_fifo_manager_v1 {{{
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

-- }}}

-- Wp_fifo_v1 {{{
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

-- }}}
-- }}}

$(generateTables False wlFormatter "xml-protocols/fifo-v1.xml")

-- vim: foldmethod=marker
