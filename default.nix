{ mkDerivation, base, criterion, fast-logger, gitrev, hspec, linear
, protolude, SDL2, sdl2, stdenv
}:
mkDerivation {
  pname = "orange-corndog";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base criterion fast-logger gitrev hspec linear protolude sdl2
  ];
  executableHaskellDepends = [
    base criterion fast-logger gitrev hspec linear protolude sdl2
  ];
  executableSystemDepends = [ SDL2 ];
  testHaskellDepends = [
    base criterion fast-logger gitrev hspec linear protolude sdl2
  ];
  homepage = "https://github.com/r-raymond/orange-corndog#readme";
  description = "A quick pathtracer";
  license = stdenv.lib.licenses.gpl3;
}
