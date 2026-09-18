module Main (main) where

import Client.BasicWindow qualified
import Client.WallpaperDaemon qualified
import Server.BasicCompositor qualified
import Test.Tasty
import Test.Tasty.HUnit
import Prelude

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [integrationTests]

integrationTests :: TestTree
integrationTests =
  testGroup
    "Integration tests"
    [ testCase "client-basic-window" Client.BasicWindow.test
    , testCase "client-wallpaper-daemon" Client.WallpaperDaemon.test
    , testCase "server-basic-compositor" Server.BasicCompositor.test
    ]
