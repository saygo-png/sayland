-- | Description : Non protocol specific object management and communication.
module Sayland.Object (newObjectID, registerObject, dropObject, newObject, getInterface, sendMsg, receiveMsg, ObjectIDsExhausted (..)) where

import Sayland.Internal.Object
import Sayland.Internal.Protocols.Wayland (dropObject, registerObject)
