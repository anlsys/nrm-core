{ mkDerivation, fetchurl, base, generic-lens, intervals, lens, MonadRandom
, protolude, random, refined, stdenv }:
mkDerivation {
  src = fetchurl {
    url =
      "https://xgitlab.cels.anl.gov/argo/hbandit/-/archive/master/hbandit-master.tar.gz";
    sha256 = "05dcwnfmn01q953rrrpadfx7ax3ppxkzvcx2y701wpyfjarsbqmv";
  };
  pname = "hbandit";
  version = "1.0.0";
  libraryHaskellDepends =
    [ base generic-lens intervals lens MonadRandom protolude random refined ];
  testHaskellDepends =
    [ base generic-lens intervals lens MonadRandom protolude random refined ];
  description = "hbandit";
  license = stdenv.lib.licenses.bsd3;
}
