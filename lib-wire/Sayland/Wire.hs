{-# LANGUAGE PatternSynonyms #-}

-- | Description : The Wayland wire format, and sending and receiving it over a socket.
--
-- Also re-exports `Int32`, `Word32` and `Fd`, which messages carry for Wayland's @int@, @uint@ and @fd@.
module Sayland.Wire (module Sayland.Internal.Wire, module Sayland.Internal.Connection, Int32, Word32, Fd) where

import Data.Int (Int32)
import Data.Word (Word32)
import Sayland.Internal.Connection
import Sayland.Internal.Wire (ObjectID, WlText)
import Sayland.Internal.Wire hiding (ObjectID, WlText, pattern WlTextString)
import System.Posix.Types (Fd)
