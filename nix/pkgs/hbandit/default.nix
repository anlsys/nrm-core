{ mkDerivation, fetchurl, base, generic-lens, intervals, lens, MonadRandom
, protolude, random, refined, stdenv }:
mkDerivation {
  src = fetchurl {
    url =
      "https://xgitlab.cels.anl.gov/argo/hbandit/-/archive/v0.1/hbandit-v0.1.tar.gz";
    sha256 = "02psclmlvz94axwyx17pzscb8c5s92d2q2lcnrrbx56kpgy3gwkw";
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
