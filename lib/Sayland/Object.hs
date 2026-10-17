-- | Description : Non protocol specific object management and communication.
module Sayland.Object (newObjectID, withNewObjectID, registerObject, dropObject, newObject, getInterface, getInterfaceSTM, sendMsg, receiveMsg, ObjectIDsExhausted (..)) where

import Sayland.Internal.Object
import Sayland.Internal.Protocols.Wayland (dropObject, registerObject)
