module Main (main) where

import Both.Wire qualified
import ContentUpdate qualified
import Integration qualified
import Races qualified
import Test.Tasty
import Prelude

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [Integration.tests, Races.tests, ContentUpdate.tests, Both.Wire.tests]
