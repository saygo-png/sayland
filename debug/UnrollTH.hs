module Main (main) where

import Language.Haskell.TH
import Sayland.Codegen
import System.Process (readProcess)
import Prelude

main :: IO ()
main = do
  ast <- runQ (loadProtocols wlFormatter True "protocols")
  let rawCode = pprint ast
  formattedCode <- readProcess "fourmolu" ["--stdin-input-file", "unrolled-TH.hs"] rawCode
  writeFile "unrolled-TH.hs" formattedCode
