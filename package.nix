{niceHaskell, ...}:
niceHaskell.mkPackage {
  flags = niceHaskell.mkFlags {
    doCheck = false;
    doHaddock = false;
  };
  packageRoot = ./.;
  cabalName = "sayland";
  compiler = "ghc912";
}
