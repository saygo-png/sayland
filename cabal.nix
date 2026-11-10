{ mkDerivation, base, binary, bytestring, containers, directory
, filepath, lib, mtl, network, process, random, relude
, shared-memory, stm, tasty, tasty-hunit, tasty-quickcheck
, template-haskell, text, unix, xml
}:
mkDerivation {
  pname = "sayland";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base binary bytestring containers directory filepath mtl network
    shared-memory stm template-haskell text unix xml
  ];
  executableHaskellDepends = [
    base bytestring containers directory filepath network process
    random relude stm text unix
  ];
  testHaskellDepends = [
    base binary bytestring containers directory network random relude
    stm tasty tasty-hunit tasty-quickcheck template-haskell unix
  ];
  doHaddock = false;
  homepage = "https://github.com/saygo-png/sayland";
  description = "Generic, simple Wayland library without C bindings";
  license = lib.meta.getLicenseFromSpdxId "GPL-3.0-or-later";
}
