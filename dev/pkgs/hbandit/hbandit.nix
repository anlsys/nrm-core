{ mkDerivation, base, generic-lens, intervals, lens, MonadRandom
, protolude, random, refined, stdenv, src
}:
mkDerivation {
  inherit src;
  pname = "hbandit";
  version = "1.0.0";
  libraryHaskellDepends = [
    base generic-lens intervals lens MonadRandom protolude random
    refined
  ];
  testHaskellDepends = [
    base generic-lens intervals lens MonadRandom protolude random
    refined
  ];
  description = "hbandit";
  license = stdenv.lib.licenses.bsd3;
}
