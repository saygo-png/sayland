{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | Description: Internals of Sayland.Protocols.WlrLayerShell
module Sayland.Internal.Protocols.WlrLayerShell (module Sayland.Internal.Protocols.WlrLayerShell) where

import Sayland.Internal.Codegen
import Sayland.Internal.Core
import Sayland.Internal.Object
import Sayland.Internal.Prelude
import Sayland.Internal.Protocols.Wayland
import Sayland.Internal.Protocols.XdgShell

$(loadProtocolFileEnums "xml-protocols/wlr-layer-shell-unstable-v1.xml")

-- Interfaces {{{
newtype Zwlr_layer_shell_v1 = Zwlr_layer_shell_v1 {wlid :: TObjectID Zwlr_layer_shell_v1}

newtype Zwlr_layer_surface_v1 = Zwlr_layer_surface_v1 {wlid :: TObjectID Zwlr_layer_surface_v1}

-- }}}

$(loadProtocolFile wlFormatter "xml-protocols/wlr-layer-shell-unstable-v1.xml")

-- Implementations {{{
-- zwlr_layer_shell_v1 {{{
instance Object Zwlr_layer_shell_v1 where
  onRequest shell msg@(Request_zwlr_layer_shell_v1_get_layer_surface layerSurfaceId _surfaceId _outputId _layer _namespace) = do
    registerObject Zwlr_layer_surface_v1{wlid = layerSurfaceId}
    forwardMessage shell msg
  onRequest shell msg@Request_zwlr_layer_shell_v1_destroy = do
    forwardMessage shell msg
    dropObject shell.wlid

  onEvent _ = \case {}

-- }}}

-- TODO: Zwlr_layer_surface_v1 {{{
instance Object Zwlr_layer_surface_v1 where
  onRequest obj msg@Request_zwlr_layer_surface_v1_destroy = do
    forwardMessage obj msg
    dropObject obj.wlid
  onRequest obj msg = do
    stub obj msg
    forwardMessage obj msg

  onEvent obj msg = do
    stub obj msg
    forwardMessage obj msg

-- }}}
-- }}}

$(generateTables wlFormatter "xml-protocols/wlr-layer-shell-unstable-v1.xml")

-- vim: foldmethod=marker
