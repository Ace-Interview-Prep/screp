{ mkDerivation, base, containers, directory, filepath, lib
, optparse-applicative, parsec, process, scrappy-core
}:
mkDerivation {
  pname = "screp";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers directory filepath parsec process scrappy-core
  ];
  executableHaskellDepends = [
    base containers directory filepath optparse-applicative scrappy-core
  ];
  testHaskellDepends = [
    base directory filepath parsec scrappy-core
  ];
  homepage = "https://github.com/Ace-Interview-Prep/screp";
  description = "grep-like CLI using Parsec parsers instead of regex";
  license = lib.licenses.bsd3;
}
