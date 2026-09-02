{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Sayland.Protocols.XdgShell (module Sayland.Protocols.XdgShell) where

import Control.Lens (makeFieldsId)
import Data.Data (cast)
import Data.Map qualified as Map
import Protocol
import Relude
import Sayland.Internal.Utils
import Sayland.Protocols.Wayland
import Sayland.Types
import Sayland.Utils

-- Interfaces {{{
$(loadProtocolFileEnums False "protocols/xdg-shell.xml")

newtype Xdg_wm_base = Xdg_wm_base {wlid :: TObjectID Xdg_wm_base}

newtype Xdg_positioner = Xdg_positioner {wlid :: TObjectID Xdg_positioner}

-- todo: xdgRole might be unnecessary, instead Wl_surface.role should be used.
data Xdg_surface = Xdg_surface {wlid :: TObjectID Xdg_surface, wl_surface :: TObjectID Wl_surface, xdgRole :: IORef (Maybe XDGRole)}

data Xdg_toplevel = Xdg_toplevel {toplevel_xdg_surface :: TObjectID Xdg_surface, wlid :: TObjectID Xdg_toplevel, size :: IORef (Int, Int), parent :: IORef (Maybe (TObjectID Xdg_toplevel))}

data Xdg_popup = Xdg_popup {popup_xdg_surface :: TObjectID Xdg_surface, wlid :: TObjectID Xdg_popup, parent :: TObjectID Xdg_surface, positioner :: TObjectID Xdg_positioner}

data XDGRole = XDGToplevel Xdg_toplevel | XDGPopup Xdg_popup

instance DefaultIO Xdg_wm_base where defM = pure $ Xdg_wm_base 0

instance DefaultIO Xdg_positioner where defM = pure $ Xdg_positioner 0

instance DefaultIO Xdg_surface where defM = newIORef Nothing <&> Xdg_surface 0 0

instance DefaultIO Xdg_toplevel where
  defM = do
    size <- newIORef (0, 0)
    parent <- newIORef Nothing
    pure Xdg_toplevel{wlid = 0, toplevel_xdg_surface = 0, ..}

instance DefaultIO Xdg_popup where defM = pure $ Xdg_popup{popup_xdg_surface = 0, wlid = 0, parent = 0, positioner = 0}

$(concat <$> mapM makeFieldsId [''Xdg_wm_base, ''Xdg_positioner, ''Xdg_surface, ''Xdg_toplevel, ''Xdg_popup])

$(loadProtocolFile wlFormatter False "protocols/xdg-shell.xml")
$(generateTables False wlFormatter "protocols/xdg-shell.xml")

-- }}}

-- Implementations {{{
instance Interface' Xdg_wm_base Client where
  type Event Xdg_wm_base = Event_xdg_wm_base
  type Request Xdg_wm_base = Request_xdg_wm_base
  runRequest wm_base request@Request_xdg_wm_base_destroy = do
    dropObject wm_base.wlid
    sendMessage' request wm_base.wlid
  runRequest wm_base request@(Request_xdg_wm_base_create_positioner positionerId) = do
    _positionerObject <- newObject positionerId Xdg_positioner{wlid = positionerId}
    sendMessage' request wm_base.wlid
  runRequest wm_base request@(Request_xdg_wm_base_get_xdg_surface xdgSurfaceId surfaceId) = do
    getInterface surfaceId >>= \case
      Just _ -> do
        -- TODO there are 3 checks to be made beforehand.
        ref <- newIORef Nothing
        _surfaceObject <- newObject xdgSurfaceId Xdg_surface{wlid = xdgSurfaceId, wl_surface = surfaceId, xdgRole = ref}
        sendMessage' request wm_base.wlid
      Nothing -> do
        error "get_xdg_surface called on a non-surface object."
  runRequest wm_base request@(Request_xdg_wm_base_pong{}) = do
    sendMessage' request wm_base.wlid
  runEvent wm_base (Event_xdg_wm_base_ping serial) = runRequest wm_base $ Request_xdg_wm_base_pong serial

instance Interface' Xdg_wm_base Server where
  type Event Xdg_wm_base = Event_xdg_wm_base
  type Request Xdg_wm_base = Request_xdg_wm_base

  runRequest _ (Request_xdg_wm_base_create_positioner positionerId) = void $ newObject positionerId Xdg_positioner{wlid = positionerId}
  runRequest wm_base Request_xdg_wm_base_destroy = dropObject wm_base.wlid
  runRequest _ (Request_xdg_wm_base_get_xdg_surface xdgSurfaceId surfaceId) = do
    getInterface surfaceId >>= \case
      Just _ -> do
        ref <- newIORef Nothing
        void $ newObject xdgSurfaceId Xdg_surface{wlid = xdgSurfaceId, xdgRole = ref, wl_surface = surfaceId}
      Nothing -> error "get_xdg_surface called on a non-surface object."
  runRequest _ (Request_xdg_wm_base_pong{}) = pass
  runEvent wm_base event@(Event_xdg_wm_base_ping{}) = sendMessage' event wm_base.wlid

instance Interface' Xdg_positioner Client where
  type Event Xdg_positioner = Event_xdg_positioner
  type Request Xdg_positioner = Request_xdg_positioner

  runRequest positioner' request@Request_xdg_positioner_destroy = do
    dropObject positioner'.wlid
    sendMessage' request positioner'.wlid
  runRequest _ _ = pass
  runEvent _ _ = pass

instance Interface' Xdg_positioner Server where
  type Event Xdg_positioner = Event_xdg_positioner
  type Request Xdg_positioner = Request_xdg_positioner
  runRequest _ _ = pass
  runEvent _ _ = pass

instance Interface' Xdg_surface Client where
  type Event Xdg_surface = Event_xdg_surface
  type Request Xdg_surface = Request_xdg_surface
  runRequest xdg_surface request@Request_xdg_surface_destroy = do
    ClientEnv env <- ask
    readIORef xdg_surface.xdgRole >>= \case
      Nothing -> delete
      Just x -> do
        let roleid = case x of
              XDGToplevel (Xdg_toplevel{wlid = TObjectID b_wlid}) -> b_wlid
              XDGPopup (Xdg_popup{wlid = TObjectID b_wlid}) -> b_wlid
        keys <- Map.keys <$> readIORef env.objects
        unless (roleid `elem` keys) delete
    where
      delete = do
        dropObject xdg_surface.wlid
        sendMessage' request xdg_surface.wlid
        surfaceObject' <- getInterface xdg_surface.wl_surface
        case surfaceObject' of
          Just surfaceObject -> writeIORef surfaceObject.role $ SurfaceRole ()
          Nothing -> pass
  runRequest xdg_surface request@(Request_xdg_surface_get_toplevel toplevelId) = do
    surfaceObj' <- getInterface xdg_surface.wl_surface
    case surfaceObj' of
      Just surfaceObj -> do
        SurfaceRole role' <- readIORef surfaceObj.role
        case cast role' of
          Just () -> do
            toplevel <- defM
            toplevelObject <- newObject toplevelId (toplevel{wlid = toplevelId, toplevel_xdg_surface = xdg_surface.wlid} :: Xdg_toplevel)
            writeIORef xdg_surface.xdgRole $ Just $ XDGToplevel toplevelObject
            sendMessage' request xdg_surface.wlid
            writeIORef surfaceObj.role $ SurfaceRole toplevelObject
          _ -> error "invalid wl_surface: already has a role"
      Nothing -> error "Request_xdg_surface_get_toplevel: invalid surface"
  runRequest xdg_surface request@(Request_xdg_surface_get_popup popupId popupParent popupPositioner) = do
    surfaceObj' <- getInterface xdg_surface.wl_surface
    case surfaceObj' of
      Just surfaceObj -> do
        SurfaceRole role' <- readIORef surfaceObj.role
        case cast role' of
          Just () -> do
            popupObject <- newObject popupId Xdg_popup{wlid = popupId, parent = popupParent, positioner = popupPositioner, popup_xdg_surface = xdg_surface.wlid}
            writeIORef xdg_surface.xdgRole $ Just $ XDGPopup popupObject
            sendMessage' request xdg_surface.wlid
            writeIORef surfaceObj.role $ SurfaceRole popupObject
          _ -> error "invalid wl_surface: already has a role"
      Nothing -> error "Request_xdg_surface_get_popup: invalid surface"
  runRequest xdg_surface request@(Request_xdg_surface_ack_configure{}) = do
    sendMessage' request xdg_surface.wlid
  runRequest _ _ = pass
  runEvent xdg_surface (Event_xdg_surface_configure serial) = do
    runRequest xdg_surface (Request_xdg_surface_ack_configure serial)

instance Interface' Xdg_surface Server where
  type Event Xdg_surface = Event_xdg_surface
  type Request Xdg_surface = Request_xdg_surface
  runRequest xdg_surface Request_xdg_surface_destroy = do
    ClientServerEnv _ env _ <- ask
    readIORef xdg_surface.xdgRole >>= \case
      Nothing -> delete
      Just x -> do
        let roleid = case x of
              XDGToplevel (Xdg_toplevel{wlid = TObjectID b_wlid}) -> b_wlid
              XDGPopup (Xdg_popup{wlid = TObjectID b_wlid}) -> b_wlid
        keys <- Map.keys <$> readIORef env.objects
        unless (roleid `elem` keys) delete
    where
      delete = do
        dropObject xdg_surface.wlid
        surfaceObject' <- getInterface xdg_surface.wl_surface
        case surfaceObject' of
          Just surfaceObject -> writeIORef surfaceObject.role $ SurfaceRole ()
          Nothing -> pass
  runRequest xdg_surface (Request_xdg_surface_get_toplevel toplevelId) = do
    surfaceObject' <- getInterface xdg_surface.wl_surface
    case surfaceObject' of
      Just surfaceObject -> do
        toplevel <- defM
        toplevelObject <- newObject toplevelId (toplevel{wlid = toplevelId, toplevel_xdg_surface = xdg_surface.wlid} :: Xdg_toplevel)
        writeIORef xdg_surface.xdgRole $ Just $ XDGToplevel toplevelObject
        writeIORef surfaceObject.role $ SurfaceRole toplevelObject
      Nothing -> sendError xdg_surface.wlid 1 "not_constructed" -- ?
  runRequest xdg_surface (Request_xdg_surface_get_popup popupId popupParent popupPositioner) = do
    surfaceObject' <- getInterface xdg_surface.wl_surface
    case surfaceObject' of
      Just surfaceObject -> do
        popupObject <- newObject popupId Xdg_popup{wlid = popupId, parent = popupParent, positioner = popupPositioner, popup_xdg_surface = xdg_surface.wlid}
        writeIORef xdg_surface.xdgRole $ Just $ XDGPopup popupObject
        writeIORef surfaceObject.role $ SurfaceRole popupObject
      Nothing -> sendError xdg_surface.wlid 1 "not_constructed"
  runRequest _ (Request_xdg_surface_ack_configure{}) = pass
  runRequest _ _ = pass
  runEvent xdg_surface event@(Event_xdg_surface_configure _) = sendMessage' event xdg_surface.wlid

instance Interface' Xdg_toplevel Client where
  type Event Xdg_toplevel = Event_xdg_toplevel
  type Request Xdg_toplevel = Request_xdg_toplevel
  runRequest _ _ = pass
  runEvent toplevel (Event_xdg_toplevel_configure w h _) = do
    writeIORef toplevel.size (w, h)
  runEvent _toplevel (Event_xdg_toplevel_configure_bounds _ _) = pass
  runEvent _ _ = pass

instance Interface' Xdg_toplevel Server where
  type Event Xdg_toplevel = Event_xdg_toplevel
  type Request Xdg_toplevel = Request_xdg_toplevel
  runRequest _ _ = pass
  runEvent toplevel event@(Event_xdg_toplevel_configure w h _) = do
    writeIORef toplevel.size (w, h)
    sendMessage' event toplevel.wlid
  runEvent toplevel event@(Event_xdg_toplevel_configure_bounds _ _) = sendMessage' event toplevel.wlid
  runEvent _ _ = pass

instance Interface' Xdg_popup Client where
  type Event Xdg_popup = Event_xdg_popup
  type Request Xdg_popup = Request_xdg_popup
  runRequest _ _ = pass
  runEvent _ _ = pass

instance Interface' Xdg_popup Server where
  type Event Xdg_popup = Event_xdg_popup
  type Request Xdg_popup = Request_xdg_popup
  runRequest _ _ = pass
  runEvent _ _ = pass

-- }}}
-- vim: foldmethod=marker
