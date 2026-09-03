{
  niceHaskell,
  doHaddock ? false,
  ...
}:
niceHaskell.mkPackage {
  flags = niceHaskell.mkFlags {
    doCheck = false;
    inherit doHaddock;
  };
  packageRoot = ./.;
  cabalName = "sayland";
  compiler = "ghc912";
}
