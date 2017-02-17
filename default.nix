{ mkDerivation, base, fast-logger, gitrev, linear, protolude
, stdenv
}:
mkDerivation {
  pname = "orange-corndog";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base fast-logger gitrev linear protolude
  ];
  executableHaskellDepends = [
    base fast-logger gitrev linear protolude
  ];
  homepage = "https://github.com/r-raymond/orange-corndog#readme";
  description = "A quick pathtracer";
  license = stdenv.lib.licenses.gpl3;
}
