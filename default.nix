{ mkDerivation, aeson, base, brick, bytestring, lens, lens-aeson
, microlens, random, scientific, stdenv, text, vector, vty, wreq
}:
mkDerivation {
  pname = "cetui";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base brick bytestring lens lens-aeson microlens random
    scientific text vector vty wreq
  ];
  license = stdenv.lib.licenses.gpl3;
}
