{niceHaskell, ...}:
niceHaskell.mkPackageNoIFD {
  src = ./.;
  cabal2nix = ./cabal.nix;
  compiler = "ghc912";
}
