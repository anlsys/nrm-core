{ mkDerivation, base, enclosed-exceptions, glpk, nrmlib, protolude
, stdenv
}:
mkDerivation {
  pname = "hsnrm";
  version = "1.0.0";
  src = /nix/store/dy7bbmwwcfq095a5gp6pxm0zr5psq0lb-patchedSrc;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base enclosed-exceptions nrmlib protolude
  ];
  executableSystemDepends = [ glpk ];
  description = "hsnrm";
  license = stdenv.lib.licenses.bsd3;
}
