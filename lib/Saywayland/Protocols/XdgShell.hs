{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Saywayland.Protocols.XdgShell(module Saywayland.Protocols.XdgShell) where

import Control.Lens (makeFieldsId)
import Data.Char (toUpper)
import Data.Map qualified as Map
import Protocol
import Relude
import Saywayland.Protocols.Wayland (WL_surface)
import Saywayland.Types

-- TemplateHaskell Definitions {{{
$(loadProtocolFile False "protocols/xdg-shell.xml")

-- }}}

-- Interfaces {{{
newtype XDG_wm_base = XDG_wm_base {wlid :: Word32}

newtype XDG_positioner = XDG_positioner {wlid :: Word32}

data XDG_surface = XDG_surface {wlid :: Word32, wl_surface :: ObjectID, role :: IORef (Maybe XDGRole)}

newtype XDG_toplevel = XDG_toplevel {wlid :: Word32}

newtype XDG_popup = XDG_popup {wlid :: Word32}

data XDGRole = XDGToplevel XDG_toplevel | XDGPopup XDG_popup

instance DefaultIO XDG_wm_base where defM = pure $ XDG_wm_base 0

instance DefaultIO XDG_positioner where defM = pure $ XDG_positioner 0

instance DefaultIO XDG_surface where defM = newIORef Nothing <&> XDG_surface 0 0

instance DefaultIO XDG_toplevel where defM = pure $ XDG_toplevel 0

instance DefaultIO XDG_popup where defM = pure $ XDG_popup 0

makeFieldsId ''XDG_wm_base
makeFieldsId ''XDG_positioner
makeFieldsId ''XDG_surface
makeFieldsId ''XDG_toplevel
makeFieldsId ''XDG_popup

-- }}}

-- Tables {{{
$(generateTables False (\(x1 : x2 : x3 : xs) -> toUpper x1 : toUpper x2 : toUpper x3 : xs) "protocols/xdg-shell.xml")

-- }}}

-- Implementations {{{
instance Interface' XDG_wm_base Client where
  type Event XDG_wm_base = Event_xdg_wm_base
  type Request XDG_wm_base = Request_xdg_wm_base
  runRequest wm_base request@Request_xdg_wm_base_destroy = do
    ClientEnv env <- ask
    modifyIORef env.objects $ Map.delete wm_base.wlid
    sendMessage' request wm_base.wlid (getOpcode request)
  runRequest wm_base request@Request_xdg_wm_base_create_positioner{id=positionerId} = do
    _positionerObject <- newObject positionerId XDG_positioner{wlid=positionerId}
    sendMessage' request wm_base.wlid (getOpcode request)
  runRequest wm_base request@Request_xdg_wm_base_get_xdg_surface{id=xdgSurfaceId, surface=surfaceId} = do
    getInterface' @WL_surface surfaceId >>= \case
      Just _ -> do
        -- TODO there are 3 checks to be made beforehand.
        ref <- newIORef Nothing
        _surfaceObject <- newObject xdgSurfaceId XDG_surface{wlid=xdgSurfaceId, wl_surface=surfaceId, role = ref}
        sendMessage' request wm_base.wlid (getOpcode request)
      Nothing -> do
        error "get_xdg_surface called on a non-surface object."
  runRequest wm_base request@Request_xdg_wm_base_pong{} = do
    sendMessage' request wm_base.wlid (getOpcode request)
  runEvent wm_base Event_xdg_wm_base_ping{serial} = runRequest wm_base Request_xdg_wm_base_pong{serial}

instance Interface' XDG_wm_base Server where
  type Event XDG_wm_base = Event_xdg_wm_base
  type Request XDG_wm_base = Request_xdg_wm_base

  runRequest _ Request_xdg_wm_base_create_positioner{id=positionerId} = void $ newObject positionerId XDG_positioner{wlid=positionerId}
  runRequest wm_base Request_xdg_wm_base_destroy = do
    ClientServerEnv _senv env <- ask
    modifyIORef env.objects $ Map.delete wm_base.wlid
  runRequest _ Request_xdg_wm_base_get_xdg_surface{id=xdgSurfaceId, surface=surfaceId} = do
    getInterface' @WL_surface surfaceId >>= \case
      Just _ -> do
        ref <- newIORef Nothing
        void $ newObject xdgSurfaceId XDG_surface{wlid=xdgSurfaceId, role = ref,wl_surface=surfaceId}
      Nothing -> error "get_xdg_surface called on a non-surface object."
  runRequest _ Request_xdg_wm_base_pong{} = pass
  runEvent wm_base event@Event_xdg_wm_base_ping{} = sendMessage' event wm_base.wlid $ getOpcode event


instance Interface' XDG_positioner Client where
  type Event XDG_positioner = Event_xdg_positioner
  type Request XDG_positioner = Request_xdg_positioner

  runRequest positioner request@Request_xdg_positioner_destroy = do
    ClientEnv env <- ask
    modifyIORef env.objects $ Map.delete positioner.wlid
    sendMessage' request positioner.wlid (getOpcode request)

  runRequest _ _ = pass
  runEvent _ _ = pass

instance Interface' XDG_positioner Server where
  type Event XDG_positioner = Event_xdg_positioner
  type Request XDG_positioner = Request_xdg_positioner
  runRequest _ _ = pass
  runEvent _ _ = pass

instance Interface' XDG_surface Client where
  type Event XDG_surface = Event_xdg_surface
  type Request XDG_surface = Request_xdg_surface
  runRequest xdg_surface request@Request_xdg_surface_destroy = do
    ClientEnv env <- ask
    readIORef xdg_surface.role >>= \case
      Nothing -> delete
      Just x -> do
        let roleid = case x of
              XDGToplevel (XDG_toplevel b_wlid) -> b_wlid
              XDGPopup (XDG_popup b_wlid) -> b_wlid
        keys <- Map.keys <$> readIORef env.objects
        unless (roleid `elem` keys) delete
    where
      delete = do
        ClientEnv env <- ask
        modifyIORef env.objects $ Map.delete xdg_surface.wlid
        sendMessage' request xdg_surface.wlid (getOpcode request)
  runRequest xdg_surface request@Request_xdg_surface_get_toplevel{id=toplevelId} = do
    toplevelObject <- newObject toplevelId XDG_toplevel{wlid=toplevelId}
    writeIORef xdg_surface.role $ Just $ XDGToplevel toplevelObject
    sendMessage' request xdg_surface.wlid (getOpcode request)
  runRequest xdg_surface request@Request_xdg_surface_get_popup{id=popupId} = do
    popupObject <- newObject popupId XDG_popup{wlid=popupId}
    writeIORef xdg_surface.role $ Just $ XDGPopup popupObject
    sendMessage' request xdg_surface.wlid (getOpcode request)
  runRequest xdg_surface request@Request_xdg_surface_ack_configure{} = do
    sendMessage' request xdg_surface.wlid (getOpcode request)
  runRequest _ _ = pass
  runEvent _ _ = pass

instance Interface' XDG_surface Server where
  type Event XDG_surface = Event_xdg_surface
  type Request XDG_surface = Request_xdg_surface
  runRequest xdg_surface Request_xdg_surface_destroy = do
    ClientServerEnv _ env <- ask
    readIORef xdg_surface.role >>= \case
      Nothing -> delete
      Just x -> do
        let roleid = case x of
              XDGToplevel (XDG_toplevel b_wlid) -> b_wlid
              XDGPopup (XDG_popup b_wlid) -> b_wlid
        keys <- Map.keys <$> readIORef env.objects
        unless (roleid `elem` keys) delete
    where
      delete = do
        ClientServerEnv _ env <- ask
        modifyIORef env.objects $ Map.delete xdg_surface.wlid
  runRequest xdg_surface Request_xdg_surface_get_toplevel{id=toplevelId} = do
    toplevelObject <- newObject toplevelId XDG_toplevel{wlid=toplevelId}
    writeIORef xdg_surface.role $ Just $ XDGToplevel toplevelObject
  runRequest xdg_surface Request_xdg_surface_get_popup{id=popupId} = do
    popupObject <- newObject popupId XDG_popup{wlid=popupId}
    writeIORef xdg_surface.role $ Just $ XDGPopup popupObject
  runRequest _ Request_xdg_surface_ack_configure{} = pass
  runRequest _ _ = pass
  runEvent _ _ = pass

instance Interface' XDG_toplevel Client where
  type Event XDG_toplevel = Event_xdg_toplevel
  type Request XDG_toplevel = Request_xdg_toplevel
  runRequest _ _ = pass
  runEvent _ _ = pass

instance Interface' XDG_toplevel Server where
  type Event XDG_toplevel = Event_xdg_toplevel
  type Request XDG_toplevel = Request_xdg_toplevel
  runRequest _ _ = pass
  runEvent _ _ = pass

instance Interface' XDG_popup Client where
  type Event XDG_popup = Event_xdg_popup
  type Request XDG_popup = Request_xdg_popup
  runRequest _ _ = pass
  runEvent _ _ = pass

instance Interface' XDG_popup Server where
  type Event XDG_popup = Event_xdg_popup
  type Request XDG_popup = Request_xdg_popup
  runRequest _ _ = pass
  runEvent _ _ = pass

-- }}}
-- vim: foldmethod=marker
