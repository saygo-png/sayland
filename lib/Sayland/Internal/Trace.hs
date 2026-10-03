-- | Description : Tracing helpers.
module Sayland.Internal.Trace (getColorize) where

import Data.String
import GHC.IO.StdHandles
import Sayland.Internal.Prelude
import System.Console.ANSI (Color (..), ColorIntensity (..), ConsoleLayer (..), SGR (..), hNowSupportsANSI, setSGRCode)

-- | Get a text coloring function. If stdout does not have ANSI support, return the @id@ function.
getColorize :: (IsString s, Semigroup s) => IO (ColorIntensity -> Color -> s -> s)
getColorize = do
  ansiSupport <- hNowSupportsANSI stdout
  pure
    $ if ansiSupport
      then \ci c t -> fromString (setSGRCode [SetColor Foreground ci c]) <> t <> fromString (setSGRCode [Reset])
      else const $ const id
