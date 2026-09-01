module Main (main) where

import Language.Haskell.TH
import Protocol
import Prelude

main :: IO ()
main = do
  runQ (loadProtocols id True "protocols") >>= writeFile "output.hs" . pprint
