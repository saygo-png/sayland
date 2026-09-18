module Main (main) where

import Control.Arrow
import Data.Function
import Data.String
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.Process
import Prelude

main :: IO ()
main = do
  rawHaddockOutput <- readProcess "cabal" ["haddock"] ""
  T.putStrLn $ processRawOutput (fromString rawHaddockOutput)
  pure ()

processRawOutput :: T.Text -> T.Text
processRawOutput =
  T.lines
    >>> filter (not . hasFalseWarning)
    >>> T.unlines
  where
    hasFalseWarning :: T.Text -> Bool
    hasFalseWarning = T.isInfixOf ".D:R:"
