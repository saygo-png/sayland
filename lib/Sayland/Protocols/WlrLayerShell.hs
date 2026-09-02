{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Sayland.Protocols.WlrLayerShell (module Sayland.Protocols.WlrLayerShell) where

import Protocol
import Relude
import Sayland.Internal.Utils
import Sayland.Protocols.Wayland
import Sayland.Protocols.XdgShell
import Sayland.Types
import Sayland.Utils

$(loadProtocolFileEnums False "protocols/wlr-layer-shell-unstable-v1.xml")

newtype Zwlr_layer_shell_v1 = Zwlr_layer_shell_v1 {wlid :: TObjectID Zwlr_layer_shell_v1}

newtype Zwlr_layer_surface_v1 = Zwlr_layer_surface_v1 {wlid :: TObjectID Zwlr_layer_surface_v1}

$(concat <$> mapM makeFieldsWithPrefix [''Zwlr_layer_shell_v1, ''Zwlr_layer_surface_v1])

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
  runEvent _shell _ = pass
  runRequest shell request@(Request_zwlr_layer_shell_v1_get_layer_surface layerSurfaceId _surfaceId _outputId _layer _namespace) = do
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
  runEvent _ls (Event_zwlr_layer_surface_v1_configure{}) = pass
  runRequest ls request@(Request_zwlr_layer_surface_v1_ack_configure _serial) = do
    sendMessage' request ls.wlid
  runRequest ls request@(Request_zwlr_layer_surface_v1_set_anchor _anchor) = do
    sendMessage' request ls.wlid
  runRequest ls request@(Request_zwlr_layer_surface_v1_set_exclusive_zone _zone) = do
    sendMessage' request ls.wlid
  runRequest ls request@(Request_zwlr_layer_surface_v1_set_size _width _height) = do
    sendMessage' request ls.wlid
  runRequest _ls Request_zwlr_layer_surface_v1_destroy = pass
  runRequest _ls (Request_zwlr_layer_surface_v1_get_popup _) = pass
  runRequest _ls (Request_zwlr_layer_surface_v1_set_keyboard_interactivity _) = pass
  runRequest _ls (Request_zwlr_layer_surface_v1_set_layer _) = pass
  runRequest _ls (Request_zwlr_layer_surface_v1_set_margin{}) = pass

instance Interface' Zwlr_layer_surface_v1 Server

-- }}}
-- vim: foldmethod=marker
