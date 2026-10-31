{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | Description: Internals of Sayland.Protocols.Fifo
module Sayland.Internal.Protocols.Fifo (module Sayland.Internal.Protocols.Fifo) where

import Control.Monad (when)
import Data.Foldable (traverse_)
import Data.Text qualified as T
import Data.Typeable (cast)
import Sayland.Internal.Codegen
import Sayland.Internal.Core
import Sayland.Internal.Object
import Sayland.Internal.Prelude
import Sayland.Internal.Protocols.Wayland
import Sayland.Wire

$(loadProtocolFileEnums "xml-protocols/fifo-v1.xml")

-- Interfaces {{{
newtype Wp_fifo_manager_v1 = Wp_fifo_manager_v1 {wlid :: TObjectID Wp_fifo_manager_v1}

data Wp_fifo_v1 = Wp_fifo_v1 {wlid :: TObjectID Wp_fifo_v1, fifoSurface :: TObjectID Wl_surface}

-- }}}

-- | The fifo barrier of a surface.
newtype FifoBarrier = FifoBarrier (TVar Bool)

-- | Whether the surface's fifo barrier is up.
-- If it doesn't exist yet, its created with a down state.
fifoBarrier :: Wl_surface -> STM (TVar Bool)
fifoBarrier surface = (\(FifoBarrier up) -> up) <$> surfaceExtension surface (FifoBarrier <$> newTVar False)

-- | Lower a surface's fifo barrier, then apply the content updates that waited for it.
--
-- A compositor offering fifo should call it after each latching deadline for the surfaces it presents. For a surface it does
-- not present, like a hidden one, it can call it at any time, as @wait_barrier@ allows.
clearFifoBarrier :: ConnectionEnv p -> Wl_surface -> STM ()
clearFifoBarrier env surface = do
  lookupSurfaceExtension surface >>= traverse_ (\(FifoBarrier up) -> writeTVar up False)
  flushContentUpdates env surface

$(loadProtocolFile wlFormatter "xml-protocols/fifo-v1.xml")

-- Implementations {{{
-- Wp_fifo_manager_v1 {{{
instance Object Wp_fifo_manager_v1 where
  onRequest manager msg@Request_wp_fifo_manager_v1_destroy =
    forwardAndDrop manager msg
  onRequest manager msg@(Request_wp_fifo_manager_v1_get_fifo fifoId surfaceId) = do
    getInterface surfaceId >>= \case
      Just _ -> do
        objects <- readTVarIO . (.objects) =<< getConnectionEnv
        let isFifoOfSurface (SomeObject object) = maybe False (\fifo -> fifo.fifoSurface == surfaceId) (cast @_ @Wp_fifo_v1 object)
        when (any isFifoOfSurface objects)
          $ protocolViolation Unrecoverable msg manager Enum_wp_fifo_manager_v1_error_already_exists "get_fifo: the surface already has a fifo object"
        registerObject msg Wp_fifo_v1{wlid = fifoId, fifoSurface = surfaceId}
        forwardMessage manager msg
      Nothing ->
        let errTxt = wlTextLossy $ "wp_fifo_manager_v1.get_fifo: surface `" <> T.show surfaceId <> "` does not exist"
         in protocolViolation Unrecoverable msg wlDisplayId Err_invalid_object errTxt

  onEvent _ = \case {}

-- }}}

-- Wp_fifo_v1 {{{
instance Object Wp_fifo_v1 where
  onRequest fifo msg@Request_wp_fifo_v1_set_barrier = do
    getInterface fifo.fifoSurface >>= \case
      Just surface -> do
        barrier <- atomically $ fifoBarrier surface
        atomicModifyIORef' surface.pendingState $ \cu -> (cu{cuOnApply = cu.cuOnApply <> [writeTVar barrier True]}, ())
        forwardMessage fifo msg
      Nothing -> mempty <$ protocolViolation Recoverable msg fifo Enum_wp_fifo_v1_error_surface_destroyed "set_barrier: the associated surface no longer exists"
  onRequest fifo msg@Request_wp_fifo_v1_wait_barrier = do
    getInterface fifo.fifoSurface >>= \case
      Just surface -> do
        barrier <- atomically $ fifoBarrier surface
        -- Constraint always ready for a synchronized subsurface per the wait_barrier spec.
        let waitForBarrier =
              Constraint
                { whenDesync = const $ not <$> readTVar barrier
                , whenSync = const $ pure True
                }
        atomicModifyIORef' surface.pendingState $ \cu -> (cu{cuConstraints = cu.cuConstraints <> [waitForBarrier]}, ())
        forwardMessage fifo msg
      Nothing -> mempty <$ protocolViolation Recoverable msg fifo Enum_wp_fifo_v1_error_surface_destroyed "wait_barrier: the associated surface no longer exists"
  onRequest fifo msg@Request_wp_fifo_v1_destroy =
    forwardAndDrop fifo msg

  onEvent _ = \case {}

-- }}}
-- }}}

$(generateTables wlFormatter "xml-protocols/fifo-v1.xml")

-- vim: foldmethod=marker
