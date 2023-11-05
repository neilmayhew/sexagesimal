{ mkDerivation, base, hspec, lib }:
mkDerivation {
  pname = "sexagesimal";
  version = "1.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base hspec ];
  description = "Conversion of numbers to and from sexagesimal (base 60) notation";
  license = lib.licenses.asl20;
  mainProgram = "sexagesimal";
}
