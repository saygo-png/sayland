{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | Description: Internals of Sayland.Protocols.ExtWorkspace
module Sayland.Internal.Protocols.ExtWorkspace (module Sayland.Internal.Protocols.ExtWorkspace) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import Data.Maybe
import Sayland.Internal.Codegen
import Sayland.Internal.Core
import Sayland.Internal.Object
import Sayland.Internal.Prelude
import Sayland.Internal.Protocols.Wayland
import Sayland.Wire

$(loadProtocolFileEnums "xml-protocols/ext-workspace-v1.xml")

-- Interfaces {{{
data Ext_workspace_manager_v1 = Ext_workspace_manager_v1 {wlid :: TObjectID Ext_workspace_manager_v1, stopped :: TVar Bool}

data Ext_workspace_group_handle_v1 = Ext_workspace_group_handle_v1
  { wlid :: TObjectID Ext_workspace_group_handle_v1
  , groupWorkspaces :: TVar [Ext_workspace_handle_v1]
  , alive :: TVar Bool
  }

data Ext_workspace_handle_v1 = Ext_workspace_handle_v1
  { wlid :: TObjectID Ext_workspace_handle_v1
  , workspaceGroups :: TVar [Ext_workspace_group_handle_v1]
  , hasId :: TVar Bool
  , alive :: TVar Bool
  }

-- | Add a workspace to a group, unless either was destroyed.
addToGroup :: Ext_workspace_group_handle_v1 -> Ext_workspace_handle_v1 -> STM ()
addToGroup group workspace = do
  groupAlive <- readTVar group.alive
  workspaceAlive <- readTVar workspace.alive
  when (groupAlive && workspaceAlive) $ do
    removeFromGroup group workspace
    modifyTVar' group.groupWorkspaces (workspace :)
    modifyTVar' workspace.workspaceGroups (group :)

-- | Remove a workspace from a group.
removeFromGroup :: Ext_workspace_group_handle_v1 -> Ext_workspace_handle_v1 -> STM ()
removeFromGroup group workspace = do
  modifyTVar' group.groupWorkspaces $ filter (\w -> w.wlid /= workspace.wlid)
  modifyTVar' workspace.workspaceGroups $ filter (\g -> g.wlid /= group.wlid)

-- | Remove all workspaces from a group.
removeAllWorkspaces :: Ext_workspace_group_handle_v1 -> STM ()
removeAllWorkspaces group = readTVar group.groupWorkspaces >>= traverse_ (removeFromGroup group)

-- | Remove a workspace from all of its groups.
removeFromAllGroups :: Ext_workspace_handle_v1 -> STM ()
removeFromAllGroups workspace = readTVar workspace.workspaceGroups >>= traverse_ (`removeFromGroup` workspace)

-- | Look up the workspace an event refers to. Only the compositor checks it exists,
-- a client can receive an event referring to a workspace it already destroyed.
getWorkspace :: (Message m) => m -> TObjectID Ext_workspace_handle_v1 -> Wayland p (Maybe Ext_workspace_handle_v1)
getWorkspace msg workspaceId = do
  workspace <- getInterface workspaceId
  when (isNothing workspace)
    . onServer
    $ protocolViolation Unrecoverable msg wlDisplayId Err_invalid_object [wl|ext_workspace_group_handle_v1: workspace does not exist|]
  pure workspace

-- }}}

$(loadProtocolFile wlFormatter "xml-protocols/ext-workspace-v1.xml")

-- Implementations {{{
-- Ext_workspace_manager_v1 {{{
instance Global Ext_workspace_manager_v1 where
  global i = Ext_workspace_manager_v1 i <$> newTVarIO False

instance Object Ext_workspace_manager_v1 where
  onRequest manager msg = do
    stopped <- atomically $ case msg of
      Request_ext_workspace_manager_v1_commit -> readTVar manager.stopped
      Request_ext_workspace_manager_v1_stop -> swapTVar manager.stopped True
    when stopped
      $ protocolViolation Recoverable msg wlDisplayId Err_invalid_object [wl|ext_workspace_manager_v1: request after stop|]
    forwardMessage manager msg

  onEvent manager msg@(Event_ext_workspace_manager_v1_workspace_group groupId) = do
    group <- atomically $ do
      groupWorkspaces <- newTVar []
      alive <- newTVar True
      pure Ext_workspace_group_handle_v1{wlid = groupId, groupWorkspaces, alive}
    registerObject msg group
    forwardMessage manager msg
  onEvent manager msg@(Event_ext_workspace_manager_v1_workspace workspaceId) = do
    workspace <- atomically $ do
      workspaceGroups <- newTVar []
      hasId <- newTVar False
      alive <- newTVar True
      pure Ext_workspace_handle_v1{wlid = workspaceId, workspaceGroups, hasId, alive}
    registerObject msg workspace
    forwardMessage manager msg
  onEvent manager msg@Event_ext_workspace_manager_v1_done =
    forwardMessage manager msg
  onEvent manager msg@Event_ext_workspace_manager_v1_finished =
    forwardAndDrop manager msg

-- }}}

-- Ext_workspace_group_handle_v1 {{{
instance Object Ext_workspace_group_handle_v1 where
  onRequest group msg@Request_ext_workspace_group_handle_v1_create_workspace{} =
    forwardMessage group msg
  onRequest group msg@Request_ext_workspace_group_handle_v1_destroy = do
    atomically $ writeTVar group.alive False >> removeAllWorkspaces group
    forwardAndDrop group msg

  onEvent group msg@Event_ext_workspace_group_handle_v1_capabilities{} =
    forwardMessage group msg
  onEvent group msg@Event_ext_workspace_group_handle_v1_output_enter{} =
    forwardMessage group msg
  onEvent group msg@Event_ext_workspace_group_handle_v1_output_leave{} =
    forwardMessage group msg
  onEvent group msg@(Event_ext_workspace_group_handle_v1_workspace_enter workspaceId) = do
    workspace <- getWorkspace msg workspaceId
    for_ workspace $ atomically . addToGroup group
    forwardMessage group msg
  onEvent group msg@(Event_ext_workspace_group_handle_v1_workspace_leave workspaceId) = do
    workspace <- getWorkspace msg workspaceId
    for_ workspace $ atomically . removeFromGroup group
    forwardMessage group msg
  onEvent group msg@Event_ext_workspace_group_handle_v1_removed = do
    workspaces <- liftIO $ readTVarIO group.groupWorkspaces
    unless (null workspaces)
      $ protocolViolation Recoverable msg group Err_implementation [wl|ext_workspace_group_handle_v1.removed: group still has workspaces|]
    atomically $ removeAllWorkspaces group
    forwardMessage group msg

-- }}}

-- Ext_workspace_handle_v1 {{{
instance Object Ext_workspace_handle_v1 where
  onRequest workspace msg@Request_ext_workspace_handle_v1_destroy = do
    atomically $ writeTVar workspace.alive False >> removeFromAllGroups workspace
    forwardAndDrop workspace msg
  onRequest workspace msg@Request_ext_workspace_handle_v1_activate =
    forwardMessage workspace msg
  onRequest workspace msg@Request_ext_workspace_handle_v1_deactivate =
    forwardMessage workspace msg
  onRequest workspace msg@(Request_ext_workspace_handle_v1_assign groupId) =
    getInterface groupId >>= \case
      Just Ext_workspace_group_handle_v1{} -> forwardMessage workspace msg
      Nothing -> protocolViolation Unrecoverable msg wlDisplayId Err_invalid_object [wl|ext_workspace_handle_v1.assign: workspace group does not exist|]
  onRequest workspace msg@Request_ext_workspace_handle_v1_remove =
    forwardMessage workspace msg

  onEvent workspace msg@Event_ext_workspace_handle_v1_id{} = do
    hadId <- atomically $ swapTVar workspace.hasId True
    when hadId
      $ protocolViolation Recoverable msg workspace Err_implementation [wl|ext_workspace_handle_v1.id: sent more than once|]
    forwardMessage workspace msg
  onEvent workspace msg@Event_ext_workspace_handle_v1_name{} =
    forwardMessage workspace msg
  onEvent workspace msg@Event_ext_workspace_handle_v1_coordinates{} =
    forwardMessage workspace msg
  onEvent workspace msg@Event_ext_workspace_handle_v1_state{} =
    forwardMessage workspace msg
  onEvent workspace msg@Event_ext_workspace_handle_v1_capabilities{} =
    forwardMessage workspace msg
  onEvent workspace msg@Event_ext_workspace_handle_v1_removed = do
    groups <- liftIO $ readTVarIO workspace.workspaceGroups
    unless (null groups)
      $ protocolViolation Recoverable msg workspace Err_implementation [wl|ext_workspace_handle_v1.removed: workspace still belongs to a group|]
    atomically $ removeFromAllGroups workspace
    forwardMessage workspace msg

-- }}}
-- }}}

$(generateTables wlFormatter "xml-protocols/ext-workspace-v1.xml")

-- vim: foldmethod=marker
