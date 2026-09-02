module Sayland.Internal.Utils (wlFormatter, getColorize, newNumbered, adata, qname, dumpDecs, makeFieldsWithPrefix) where

import Control.Lens (DefName (..), FieldNamer, classIdFields, classIdNamer, lensField, makeLensesWith, (.~))
import Data.Char (toUpper)
import Language.Haskell.TH
import Relude
import System.Console.ANSI (Color (..), ColorIntensity (..), ConsoleLayer (..), SGR (..), hNowSupportsANSI, setSGRCode)
import Text.XML.Light

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

adata :: Name
adata = mkName "_additionalData"

qname :: String -> QName
qname x = QName x Nothing Nothing

dumpDecs :: DecsQ -> IO ()
dumpDecs q = do
  ds <- runQ q
  mapM_ print ds

makeFieldsWithPrefix :: Name -> DecsQ
makeFieldsWithPrefix = makeLensesWith (classIdFields & lensField .~ lPrefixNamer)
  where
    lPrefixNamer :: FieldNamer
    lPrefixNamer tyName fieldNames fieldName =
      [ case d of
          MethodName cls m -> MethodName cls (prefixL m)
          TopName m -> TopName (prefixL m)
      | d <- classIdNamer tyName fieldNames fieldName
      ]
      where
        prefixL n = case nameBase n of
          (c : cs) -> mkName ('l' : toUpper c : cs)
          [] -> n
