{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | Description: Internals of Sayland.Protocols.XdgShell
module Sayland.Internal.Protocols.XdgShell (module Sayland.Internal.Protocols.XdgShell) where

import Control.Monad
import Data.Data (cast)
import Data.Int
import Data.Maybe
import Sayland.Internal.Codegen
import Sayland.Internal.Core
import Sayland.Internal.Object
import Sayland.Internal.Prelude
import Sayland.Internal.Protocols.Wayland
import Sayland.Wire

$(loadProtocolFileEnums False "xml-protocols/xdg-shell.xml")

-- Interfaces {{{

newtype Xdg_wm_base = Xdg_wm_base {wlid :: TObjectID Xdg_wm_base}

-- TODO: xdgRole might be unnecessary, instead Wl_surface.role should be used.
data Xdg_surface = Xdg_surface {wlid :: TObjectID Xdg_surface, wl_surface :: TObjectID Wl_surface, xdgRole :: IORef (Maybe XDGRole)}

data Xdg_toplevel = Xdg_toplevel
  { toplevel_xdg_surface :: TObjectID Xdg_surface
  , wlid :: TObjectID Xdg_toplevel
  , size :: IORef (Int32, Int32)
  , parent :: IORef (Maybe (TObjectID Xdg_toplevel))
  }

data Xdg_popup = Xdg_popup
  { popup_xdg_surface :: TObjectID Xdg_surface
  , wlid :: TObjectID Xdg_popup
  , parent :: TObjectID Xdg_surface
  , positioner :: TObjectID Xdg_positioner
  }

data XDGRole = XDGToplevel Xdg_toplevel | XDGPopup Xdg_popup

data Xdg_positioner = Xdg_positioner {wlid :: TObjectID Xdg_positioner, state :: IORef PositionerState}

-- | State tracking for  `Xdg_positioner`. 'Nothing' means the protocol default.
data PositionerState = PositionerState
  { psSize, psParentSize :: Maybe (Int32, Int32)
  , psAnchorRect :: Maybe Rectangle
  , psAnchor :: Maybe Enum_xdg_positioner_anchor
  , psGravity :: Maybe Enum_xdg_positioner_gravity
  , psConstraintAdjustment :: Maybe Enum_xdg_positioner_constraint_adjustment
  , psOffset :: Maybe (Int32, Int32)
  , psReactive :: Bool
  , psParentConfigure :: Maybe WlUInt
  }

-- }}}

$(loadProtocolFile wlFormatter False "xml-protocols/xdg-shell.xml")

-- Implementations {{{
-- Xdg_wm_base {{{

instance Object Xdg_wm_base where
  onRequest wm_base msg@Request_xdg_wm_base_destroy = do
    forwardMessage wm_base msg
    dropObject wm_base.wlid
  onRequest wm_base msg@(Request_xdg_wm_base_create_positioner positionerId) = do
    state <- newIORef $ PositionerState Nothing Nothing Nothing Nothing Nothing Nothing Nothing False Nothing
    registerObject Xdg_positioner{wlid = positionerId, state}
    forwardMessage wm_base msg
  onRequest wm_base msg@(Request_xdg_wm_base_get_xdg_surface xdgSurfaceId surfaceId) = do
    getInterface surfaceId >>= \case
      Just _ -> do
        -- TODO there are 3 checks to be made beforehand.
        ref <- newIORef Nothing
        registerObject Xdg_surface{wlid = xdgSurfaceId, wl_surface = surfaceId, xdgRole = ref}
        forwardMessage wm_base msg
      Nothing -> protocolErrorG Err_invalid_object "xdg_wm_base: get_xdg_surface called on a non-surface object"
  onRequest wm_base msg@Request_xdg_wm_base_pong{} =
    forwardMessage wm_base msg

  onEvent wm_base msg@(Event_xdg_wm_base_ping serial) = do
    forwardMessage wm_base msg
    onClient $ sendMsg wm_base (Request_xdg_wm_base_pong serial)

-- }}}

-- Xdg_positioner {{{

instance Object Xdg_positioner where
  onRequest positioner msg@Request_xdg_positioner_destroy = do
    forwardMessage positioner msg
    dropObject positioner.wlid
  onRequest positioner msg = do
    let set f = atomicModifyIORef' positioner.state $ \s -> (f s, ())
        invalidInput = protocolError positioner Enum_xdg_positioner_error_invalid_input
    case msg of
      Request_xdg_positioner_set_size (WlInt w) (WlInt h) -> do
        when (w <= 0 || h <= 0) $ invalidInput "xdg_positioner.set_size: size must be positive"
        set $ \s -> s{psSize = Just (w, h)}
      Request_xdg_positioner_set_anchor_rect (WlInt x) (WlInt y) (WlInt w) (WlInt h) -> do
        when (w < 0 || h < 0) $ invalidInput "xdg_positioner.set_anchor_rect: size must not be negative"
        set $ \s -> s{psAnchorRect = Just Rectangle{position = (x, y), size = (w, h)}}
      Request_xdg_positioner_set_anchor a -> set $ \s -> s{psAnchor = Just a}
      Request_xdg_positioner_set_gravity g -> set $ \s -> s{psGravity = Just g}
      Request_xdg_positioner_set_constraint_adjustment c -> set $ \s -> s{psConstraintAdjustment = Just c}
      Request_xdg_positioner_set_offset (WlInt x) (WlInt y) -> set $ \s -> s{psOffset = Just (x, y)}
      Request_xdg_positioner_set_reactive -> set $ \s -> s{psReactive = True}
      Request_xdg_positioner_set_parent_size (WlInt w) (WlInt h) -> set $ \s -> s{psParentSize = Just (w, h)}
      Request_xdg_positioner_set_parent_configure serial -> set $ \s -> s{psParentConfigure = Just serial}
    forwardMessage positioner msg

  onEvent _ = \case {}

-- }}}

-- Xdg_surface {{{

instance Object Xdg_surface where
  onRequest xdg_surface msg@Request_xdg_surface_destroy = do
    roleAlive <-
      readIORef xdg_surface.xdgRole >>= \case
        Nothing -> pure False
        Just (XDGToplevel toplevel) -> isJust <$> getInterface toplevel.wlid
        Just (XDGPopup popup) -> isJust <$> getInterface popup.wlid
    when roleAlive
      $ protocolError xdg_surface Enum_xdg_surface_error_defunct_role_object "destroyed before its role object"
    forwardMessage xdg_surface msg
    dropObject xdg_surface.wlid
    getInterface xdg_surface.wl_surface >>= \case
      Just surfaceObj -> atomicWriteIORef surfaceObj.role $ SurfaceRole ()
      Nothing -> pass
  onRequest xdg_surface msg@(Request_xdg_surface_get_toplevel toplevelId) = do
    surfaceObj <- getInterface xdg_surface.wl_surface >>= maybe (protocolErrorG Err_invalid_object "xdg_surface: wl_surface no longer exists") pure
    SurfaceRole role <- readIORef surfaceObj.role
    unless (isJust (cast role :: Maybe ()))
      $ protocolError xdg_surface Enum_xdg_surface_error_already_constructed "surface already has a role"
    size <- newIORef (0, 0)
    parent <- newIORef Nothing
    let toplevelObject = Xdg_toplevel{wlid = toplevelId, toplevel_xdg_surface = xdg_surface.wlid, size, parent}
    registerObject toplevelObject
    atomicWriteIORef xdg_surface.xdgRole $ Just $ XDGToplevel toplevelObject
    atomicWriteIORef surfaceObj.role $ SurfaceRole toplevelObject
    forwardMessage xdg_surface msg
  onRequest xdg_surface msg@(Request_xdg_surface_get_popup popupId popupParent popupPositioner) = do
    surfaceObj <- getInterface xdg_surface.wl_surface >>= maybe (protocolErrorG Err_invalid_object "xdg_surface: wl_surface no longer exists") pure
    SurfaceRole role <- readIORef surfaceObj.role
    unless (isJust (cast role :: Maybe ()))
      $ protocolError xdg_surface Enum_xdg_surface_error_already_constructed "xdg_surface: surface already has a role"
    let popupObject = Xdg_popup{wlid = popupId, parent = popupParent, positioner = popupPositioner, popup_xdg_surface = xdg_surface.wlid}
    registerObject popupObject
    atomicWriteIORef xdg_surface.xdgRole $ Just $ XDGPopup popupObject
    atomicWriteIORef surfaceObj.role $ SurfaceRole popupObject
    forwardMessage xdg_surface msg
  onRequest xdg_surface msg@Request_xdg_surface_set_window_geometry{} = do
    stub xdg_surface msg
    forwardMessage xdg_surface msg
  onRequest xdg_surface msg@Request_xdg_surface_ack_configure{} =
    forwardMessage xdg_surface msg

  onEvent xdg_surface msg@(Event_xdg_surface_configure serial) = do
    forwardMessage xdg_surface msg
    onClient $ sendMsg xdg_surface (Request_xdg_surface_ack_configure serial)

-- }}}

-- Xdg_toplevel {{{

instance Object Xdg_toplevel where
  onRequest toplevel msg@Request_xdg_toplevel_destroy = do
    forwardMessage toplevel msg
    dropObject toplevel.wlid
  onRequest toplevel msg = do
    case msg of
      -- A null parent (id 0) unsets it.
      Request_xdg_toplevel_set_parent parent -> atomicWriteIORef toplevel.parent $ if toRawObjectID parent == 0 then Nothing else Just parent
      _ -> stub toplevel msg
    forwardMessage toplevel msg

  onEvent toplevel msg = do
    case msg of
      Event_xdg_toplevel_configure (WlInt w) (WlInt h) _ -> atomicWriteIORef toplevel.size (w, h)
      _ -> stub toplevel msg
    forwardMessage toplevel msg

-- }}}

-- Xdg_popup {{{

instance Object Xdg_popup where
  onRequest popup msg@Request_xdg_popup_destroy = do
    forwardMessage popup msg
    dropObject popup.wlid
  onRequest popup msg = do
    stub popup msg
    forwardMessage popup msg

  onEvent obj msg = do
    stub obj msg
    forwardMessage obj msg

-- }}}
-- }}}

$(generateTables False wlFormatter "xml-protocols/xdg-shell.xml")

-- vim: foldmethod=marker
