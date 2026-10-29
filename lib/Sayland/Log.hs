-- | Description : Logging.
--
-- Every environment carries a t'Logger'. Inside the Wayland monad, log with `Sayland.Core.logInfo` and friends.
module Sayland.Log (
  LogLevel (..),
  Logger (..),
  Direction (..),
  ClientID,
  logWith,
  defaultLogger,
  silentLogger,
  stderrLogger,
) where

import Sayland.Internal.Core (defaultLogger)
import Sayland.Internal.Log
