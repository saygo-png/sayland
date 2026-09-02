module Sayland.Internal.Utils (wlFormatter, getColorize, newNumbered) where

import Data.Char (toUpper)
import Relude
import System.Console.ANSI (Color (..), ColorIntensity (..), ConsoleLayer (..), SGR (..), hNowSupportsANSI, setSGRCode)

wlFormatter :: String -> String
wlFormatter [] = []
wlFormatter (x : xs) = toUpper x : xs

getColorize :: (IsString s, Semigroup s) => IO (ColorIntensity -> Color -> s -> s)
getColorize = do
  ansiSupport <- hNowSupportsANSI stdout
  pure
    $ if ansiSupport
      then \ci c t -> fromString (setSGRCode [SetColor Foreground ci c]) <> t <> fromString (setSGRCode [Reset])
      else const $ const id

newNumbered :: (FilePath -> IO Bool) -> FilePath -> Int -> Int -> IO (Maybe FilePath)
newNumbered req s i maxi = bool (req this >>= bool (newNumbered req s (i + 1) maxi) (pure $ Just this)) (pure Nothing) (maxi < i)
  where
    this = s <> fromString (show i)
