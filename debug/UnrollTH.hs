-- | Description : Write out the code the protocol splices generate, as GHC dumps it while compiling them.
module Main (main) where

import Data.Bool (bool)
import Data.List (isPrefixOf, isSuffixOf, sort)
import System.Directory (doesDirectoryExist, listDirectory, removePathForcibly)
import System.FilePath (takeExtension, (</>))
import System.Process (callProcess, readProcess)
import Prelude

-- | Built apart from the usual build.
buildDir :: FilePath
buildDir = "dist-newstyle/unroll-th"

dumpDir :: FilePath
dumpDir = buildDir </> "dumps"

main :: IO ()
main = do
  -- GHCi runs every splice on each load, and `-e` fails when a module does not compile.
  removePathForcibly dumpDir
  callProcess "cabal" ["repl", "lib:sayland", "-O0", "--builddir", buildDir, "--repl-options=-e :q -ddump-splices -ddump-to-file -dsuppress-module-prefixes -dumpdir " <> dumpDir]
  dumps <- sort . filter ((== ".dump-splices") . takeExtension) <$> listFilesRecursive dumpDir
  rawCode <- concatMap (unlines . declarations . lines) <$> traverse readFile dumps
  formattedCode <- readProcess "fourmolu" ["--stdin-input-file", "unrolled-TH.hs", "--ghc-opt", "-XMagicHash"] rawCode
  writeFile "unrolled-TH.hs" formattedCode

-- | The declaration splices of a dump, each under a comment with where it is and what it splices.
-- A dump has a header line for every splice, followed by the splice, @======>@ and what it generated, all indented.
-- Expression splices are only the @wl@ quasiquoter, and left out.
declarations :: [String] -> [String]
declarations [] = []
declarations (header : rest) = case break (== "  ======>") splice of
  (source, _ : generated)
    | "Splicing declarations" `isSuffixOf` header ->
        ("-- " <> header) : (("-- " <>) . dropWhile (== ' ') <$> source) <> (drop 4 <$> generated) <> [""] <> declarations next
  _ -> declarations next
  where
    (splice, next) = break (\l -> not (null l || " " `isPrefixOf` l)) rest

listFilesRecursive :: FilePath -> IO [FilePath]
listFilesRecursive dir = concat <$> (traverse (visit . (dir </>)) =<< listDirectory dir)
  where
    visit path = doesDirectoryExist path >>= bool (pure [path]) (listFilesRecursive path)
