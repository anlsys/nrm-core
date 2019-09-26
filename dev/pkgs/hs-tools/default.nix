{ mkDerivation, stdenv, cabal-install, apply-refact, hdevtools, Glob, hindent, fswatch, hlint, protolude, shake, Cabal, fix-imports, ghcid, typed-process, optparse-applicative, unix, cabal-helper
}:
mkDerivation {
  pname = "dummy";
  version = "";
  src = "";
  libraryHaskellDepends = [
    cabal-install
    #apply-refact
    hdevtools
    #hindent
    #fswatch
    hlint
    #protolude
    fix-imports
    optparse-applicative
    shake
    Cabal
    Glob
    ghcid
    #typed-process
    #unix
  ];
  description = "";
  license = stdenv.lib.licenses.mit;
}
