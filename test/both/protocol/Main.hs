module Main (main) where

import Language.Haskell.TH
import Protocol
import Prelude
import Saywayland (wlFormatter)

main :: IO ()
main = do
  runQ (loadProtocols wlFormatter True "protocols") >>= writeFile "output.hs" . pprint
