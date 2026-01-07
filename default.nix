{ mkDerivation, base, hspec, lib, optparse-applicative
, terminal-size
}:
mkDerivation {
  pname = "sexagesimal";
  version = "1.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ];
  executableHaskellDepends = [
    base optparse-applicative terminal-size
  ];
  testHaskellDepends = [ base hspec ];
  description = "Conversion of numbers to and from sexagesimal (base 60) notation";
  license = lib.licenses.asl20;
  mainProgram = "sexagesimal";
}
