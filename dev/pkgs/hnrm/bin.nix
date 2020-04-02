{ mkDerivation, base, enclosed-exceptions, nrmlib, protolude, src
, stdenv
}:
mkDerivation {
  pname = "hsnrm";
  version = "1.0.0";
  inherit src;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base enclosed-exceptions nrmlib protolude
  ];
  description = "hsnrm";
  license = stdenv.lib.licenses.bsd3;
}
