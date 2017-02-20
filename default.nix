{ mkDerivation, base, criterion, fast-logger, gitrev, hspec, linear
, protolude, stdenv
}:
mkDerivation {
  pname = "orange-corndog";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base criterion fast-logger gitrev hspec linear protolude
  ];
  executableHaskellDepends = [
    base criterion fast-logger gitrev hspec linear protolude
  ];
  testHaskellDepends = [
    base criterion fast-logger gitrev hspec linear protolude
  ];
  homepage = "https://github.com/r-raymond/orange-corndog#readme";
  description = "A quick pathtracer";
  license = stdenv.lib.licenses.gpl3;
}
