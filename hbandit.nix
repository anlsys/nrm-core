{ mkDerivation, base, generic-lens, intervals, lens, list-extras
, MonadRandom, protolude, random, refined, stdenv
}:
mkDerivation {
  pname = "hbandit";
  version = "1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base generic-lens intervals lens list-extras MonadRandom protolude
    random refined
  ];
  description = "hbandit";
  license = stdenv.lib.licenses.bsd3;
}
