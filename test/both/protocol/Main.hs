module Main (main) where

import Language.Haskell.TH
import Protocol
import Saywayland.Internal.Utils (wlFormatter)
import Prelude

main :: IO ()
main = do
  runQ (loadProtocols wlFormatter True "protocols") >>= writeFile "output.hs" . pprint
