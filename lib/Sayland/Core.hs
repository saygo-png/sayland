-- | Description : Core of the library that most other modules depend on.
module Sayland.Core (
  Wayland,
  Perspective (..),
  GlobalName (..),
  Object (..),
  TObjectID,
  Outgoing,
  Incoming,
  Global (..),
  ClientEnvironment (..),
  ServerEnvironment (..),
  KnownPerspective (..),
  EventHandler (..),
  Interface (..),
  SomeObject (..),
  Message (..),
  WaylandEnv (..),
  InterfaceEntry (..),
  ProtocolTable,
  nullObjectID,
) where

import Sayland.Internal.Core
