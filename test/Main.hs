module Main (main) where

import Both.Wire qualified
import Integration qualified
import Test.Tasty
import Prelude

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [Integration.tests, Both.Wire.tests]
