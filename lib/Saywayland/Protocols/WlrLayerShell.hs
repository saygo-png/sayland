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
import Saywayland.Protocols.Wayland
import Saywayland.Protocols.XdgShell

$(loadProtocolFileEnums False "protocols/wlr-layer-shell-unstable-v1.xml")

newtype Zwlr_layer_shell_v1 = Zwlr_layer_shell_v1 {wlid :: TObjectID Zwlr_layer_shell_v1}
newtype Zwlr_layer_surface_v1 = Zwlr_layer_surface_v1 {wlid :: TObjectID Zwlr_layer_surface_v1}

$(concat <$> mapM makeFieldsId [''Zwlr_layer_shell_v1, ''Zwlr_layer_surface_v1])

instance DefaultIO Zwlr_layer_shell_v1 where
  defM = pure $ Zwlr_layer_shell_v1 0

instance DefaultIO Zwlr_layer_surface_v1 where
  defM = pure $ Zwlr_layer_surface_v1 0

$(loadProtocolFile wlFormatter False "protocols/wlr-layer-shell-unstable-v1.xml")
$(generateTables False wlFormatter "protocols/wlr-layer-shell-unstable-v1.xml")

-- zwlr_layer_shell_v1 {{{
instance Interface' Zwlr_layer_shell_v1 Client where
  type Event Zwlr_layer_shell_v1 = Event_zwlr_layer_shell_v1
  type Request Zwlr_layer_shell_v1 = Request_zwlr_layer_shell_v1
  runEvent shell _ = pass
  runRequest shell request@(Request_zwlr_layer_shell_v1_get_layer_surface layerSurfaceId surfaceId outputId layer namespace) = do
    sendMessage' request shell.wlid
    void $ newObject layerSurfaceId $ Zwlr_layer_surface_v1{wlid = layerSurfaceId}
  runRequest _shell Request_zwlr_layer_shell_v1_destroy = pass

instance Interface' Zwlr_layer_shell_v1 Server

-- }}}
-- zwlr_layer_surface_v1 {{{
instance Interface' Zwlr_layer_surface_v1 Client where
  type Event Zwlr_layer_surface_v1 = Event_zwlr_layer_surface_v1
  type Request Zwlr_layer_surface_v1 = Request_zwlr_layer_surface_v1
  runEvent _ls Event_zwlr_layer_surface_v1_closed = pass
  runEvent _ls (Event_zwlr_layer_surface_v1_configure _ _ _) = pass
  runRequest ls request@(Request_zwlr_layer_surface_v1_ack_configure serial) = do
    sendMessage' request ls.wlid
  runRequest ls request@(Request_zwlr_layer_surface_v1_set_anchor anchor) = do
    sendMessage' request ls.wlid
  runRequest ls request@(Request_zwlr_layer_surface_v1_set_exclusive_zone zone) = do
    sendMessage' request ls.wlid
  runRequest ls request@(Request_zwlr_layer_surface_v1_set_size width height) = do
    sendMessage' request ls.wlid
  runRequest _ls (Request_zwlr_layer_surface_v1_destroy) = pass
  runRequest _ls (Request_zwlr_layer_surface_v1_get_popup _) = pass
  runRequest _ls (Request_zwlr_layer_surface_v1_set_keyboard_interactivity _) = pass
  runRequest _ls (Request_zwlr_layer_surface_v1_set_layer _) = pass
  runRequest _ls (Request_zwlr_layer_surface_v1_set_margin _ _ _ _) = pass

instance Interface' Zwlr_layer_surface_v1 Server

-- }}}
-- vim: foldmethod=marker
