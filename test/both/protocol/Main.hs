module Main (main) where

import Language.Haskell.TH
import Sayland.Codegen
import Sayland.Internal.Utils (wlFormatter)
import Prelude

main :: IO ()
main = do
  runQ (loadProtocols wlFormatter True "protocols") >>= writeFile "output.hs" . pprint
