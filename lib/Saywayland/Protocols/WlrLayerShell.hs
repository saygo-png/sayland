{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Saywayland.Protocols.WlrLayerShell where

import Control.Lens
import Data.Binary.Put (runPut)
import Data.Char (toUpper)
import Data.Map qualified as Map
import Protocol
import Relude
import Saywayland.Types

$(loadProtocolFile False "protocols/wlr-layer-shell-unstable-v1.xml")

data Zwlr_layer_shell_v1 = Zwlr_layer_shell_v1 {wlid :: Word32}

makeFieldsId ''Zwlr_layer_shell_v1

data Zwlr_layer_surface_v1 = Zwlr_layer_surface_v1 {wlid :: Word32}

makeFieldsId ''Zwlr_layer_surface_v1

instance DefaultIO Zwlr_layer_shell_v1 where
  defM = pure $ Zwlr_layer_shell_v1 0

instance DefaultIO Zwlr_layer_surface_v1 where
  defM = pure $ Zwlr_layer_surface_v1 0

$(generateTables False (\(x1 : xs) -> toUpper x1 : xs) "protocols/wlr-layer-shell-unstable-v1.xml")

-- zwlr_layer_shell_v1 {{{
instance Interface' Zwlr_layer_shell_v1 Client where
  type Event Zwlr_layer_shell_v1 = Event_zwlr_layer_shell_v1
  type Request Zwlr_layer_shell_v1 = Request_zwlr_layer_shell_v1
  runEvent shell _ = pass
  runRequest shell request@Request_zwlr_layer_shell_v1_get_layer_surface{id = layerSurfaceId, surface = surfaceId, output = outputId, layer, namespace} = do
    ClientEnv env <- ask
    sendMessage' request shell.wlid
    modifyIORef env.objects $ Map.insert layerSurfaceId $ Interface Zwlr_layer_surface_v1{wlid = layerSurfaceId}
  runRequest shell Request_zwlr_layer_shell_v1_destroy = pass

instance Interface' Zwlr_layer_shell_v1 Server

-- }}}
-- zwlr_layer_surface_v1 {{{
instance Interface' Zwlr_layer_surface_v1 Client where
  type Event Zwlr_layer_surface_v1 = Event_zwlr_layer_surface_v1
  type Request Zwlr_layer_surface_v1 = Request_zwlr_layer_surface_v1
  runEvent ls Event_zwlr_layer_surface_v1_closed = pass
  runEvent ls Event_zwlr_layer_surface_v1_configure{} = pass
  runRequest ls request@Request_zwlr_layer_surface_v1_ack_configure{serial} = do
    sendMessage' request ls.wlid
  runRequest ls request@Request_zwlr_layer_surface_v1_set_anchor{anchor} = do
    sendMessage' request ls.wlid
  runRequest ls request@Request_zwlr_layer_surface_v1_set_exclusive_zone{zone} = do
    sendMessage' request ls.wlid
  runRequest ls request@Request_zwlr_layer_surface_v1_set_size{width, height} = do
    sendMessage' request ls.wlid
  runRequest ls Request_zwlr_layer_surface_v1_destroy{} = pass
  runRequest ls Request_zwlr_layer_surface_v1_get_popup{} = pass
  runRequest ls Request_zwlr_layer_surface_v1_set_keyboard_interactivity{} = pass
  runRequest ls Request_zwlr_layer_surface_v1_set_layer{} = pass
  runRequest ls Request_zwlr_layer_surface_v1_set_margin{} = pass

instance Interface' Zwlr_layer_surface_v1 Server

-- }}}
-- vim: foldmethod=marker
