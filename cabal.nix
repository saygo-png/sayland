{ mkDerivation, base, binary, bytestring, concurrency, containers
, dejafu, directory, exceptions, filepath, lib, mtl, process
, random, relude, tasty, tasty-hunit, tasty-quickcheck
, template-haskell, text, unix, xml
}:
mkDerivation {
  pname = "sayland";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base binary bytestring concurrency containers dejafu exceptions
    filepath mtl template-haskell text unix xml
  ];
  executableHaskellDepends = [
    base directory filepath process text
  ];
  testHaskellDepends = [
    base binary bytestring containers dejafu exceptions random relude
    tasty tasty-hunit tasty-quickcheck template-haskell unix
  ];
  doHaddock = false;
  homepage = "https://github.com/saygo-png/sayland";
  description = "Generic, simple Wayland library without C bindings";
  license = lib.meta.getLicenseFromSpdxId "GPL-3.0-or-later";
}
