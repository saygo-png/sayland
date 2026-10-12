-- | Description : Build the documentation of sayland and sayland-wire as one site.
--
-- Usage: @cabal run genHaddocks -- [output directory]@, which defaults to @docs-html@.
module Main (main) where

import Control.Monad
import Data.Bool (bool)
import Data.Foldable (for_)
import Data.List (isSuffixOf)
import Data.String
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.IO.Utf8 qualified as Utf8
import System.Directory (copyFile, doesDirectoryExist, listDirectory)
import System.Environment (getArgs)
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.Process
import Prelude

main :: IO ()
main = do
  output <-
    getArgs >>= \case
      [] -> pure "docs-html"
      [dir] -> pure dir
      _ -> fail "usage: genHaddocks [output directory]"
  void $ readProcess "cabal" ["haddock", "lib:sayland-wire"] ""
  placeWireInterface
  rawHaddockOutput <-
    readProcess "cabal" ["haddock-project", "--hackage", "--output=" <> output, "lib:sayland", "lib:sayland-wire"] ""
  T.putStrLn $ processRawOutput (fromString rawHaddockOutput)
  fixWireLinks output

-- | Remove bogus warnings: those related to https://github.com/haskell/haddock/issues/1071, the headers
-- they leave without entries, and those about modules exporting themselves with @module M (module M)@.
processRawOutput :: T.Text -> T.Text
processRawOutput = T.unlines . dropEmptyHeaders . filter (not . bogus) . T.lines
  where
    bogus line = ".D:R:" `T.isInfixOf` line || exportsItself line
    exportsItself line = case T.splitOn ": " line of
      ["Warning", m, "Could not find documentation for exported module", m'] -> m == m'
      _ -> False
    dropEmptyHeaders (header : rest)
      | "could not find link destinations for:" `T.isSuffixOf` T.stripEnd header
      , not (any ("\t- " `T.isPrefixOf`) (take 1 rest)) =
          dropEmptyHeaders rest
    dropEmptyHeaders (line : rest) = line : dropEmptyHeaders rest
    dropEmptyHeaders [] = []

-- | Cabal registers sayland-wire's Haddock interface one directory above where Haddock writes it,
-- so documenting sayland does not find it: sayland's pages lose the docs of everything they
-- re-export from sayland-wire. Copy the interface to where Cabal registered it.
placeWireInterface :: IO ()
placeWireInterface = do
  interfaces <- filter isWireInterface <$> findFiles "dist-newstyle"
  when (null interfaces) $ fail "genHaddocks: sayland-wire's .haddock file was not found in dist-newstyle"
  for_ interfaces $ \i -> copyFile i (takeDirectory (takeDirectory i) </> "sayland-wire.haddock")
  where
    isWireInterface f = takeFileName f == "sayland-wire.haddock" && takeFileName (takeDirectory f) == "sayland-wire"

-- | All files under a directory.
findFiles :: FilePath -> IO [FilePath]
findFiles dir = do
  entries <- map (dir </>) <$> listDirectory dir
  concat <$> forM entries (\e -> doesDirectoryExist e >>= bool (pure [e]) (findFiles e))

-- | Haddock links sayland's pages to sayland-wire as if it were a dependency on Hackage.
-- Point those links to the sayland-wire docs on this site instead.
fixWireLinks :: FilePath -> IO ()
fixWireLinks output = do
  let sayland = output </> "sayland"
  saylandFiles <- filter (\f -> ".html" `isSuffixOf` f || f == "doc-index.json") <$> listDirectory sayland
  for_ saylandFiles $ \f -> rewrite (sayland </> f) "sayland-wire/"
  -- Links in the project-wide quick-jump index are relative to the output directory.
  rewrite (output </> "doc-index.json") "sayland/sayland-wire/"
  where
    rewrite path target = Utf8.readFile path >>= Utf8.writeFile path . replaceHackageLinks target

-- | Replace each @https://hackage.haskell.org/package/sayland-VERSION/docs/@ with the given text.
replaceHackageLinks :: T.Text -> T.Text -> T.Text
replaceHackageLinks target text = case T.breakOn prefix text of
  (before, rest)
    | T.null rest -> before
    | (version, afterVersion) <- T.span (`elem` ['0' .. '9'] <> ".") (T.drop (T.length prefix) rest)
    , not (T.null version)
    , Just afterDocs <- T.stripPrefix "/docs/" afterVersion ->
        before <> target <> replaceHackageLinks target afterDocs
    | otherwise -> before <> prefix <> replaceHackageLinks target (T.drop (T.length prefix) rest)
  where
    prefix :: T.Text
    prefix = "https://hackage.haskell.org/package/sayland-"
