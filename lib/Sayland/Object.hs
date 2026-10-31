-- | Description : Non protocol specific object management and communication.
module Sayland.Object (newObjectID, withNewObjectID, registerObject, dropObject, forwardAndDrop, newObject, getInterface, getInterfaceSTM, sendMsg, emitMsg, receiveMsg, ObjectIDsExhausted (..)) where

import Sayland.Internal.Object
import Sayland.Internal.Protocols.Wayland (dropObject, forwardAndDrop, registerObject)
