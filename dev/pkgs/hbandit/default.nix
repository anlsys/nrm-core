{ mkDerivation, fetchurl, base, generic-lens, intervals, lens, MonadRandom
, protolude, random, refined, stdenv }:
mkDerivation {
  src = fetchurl {
    url =
      "https://xgitlab.cels.anl.gov/argo/hbandit/-/archive/master/hbandit-master.tar.gz";
    sha256 = "05060rg02nqcmvgwb3ywk4psc8xqqma4hwqd2d2bb3clmr0izdmf";
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
