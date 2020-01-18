{ haskell, mkDerivation, stdenv, cabal-install, apply-refact, hdevtools, Glob
, hindent, fswatch, hlint, protolude, shake, Cabal, fix-imports, ghcid
, typed-process, optparse-applicative, unix, cabal-helper, dhall-json }:
let
  ghcide = (import (builtins.fetchTarball
    "https://github.com/hercules-ci/ghcide-nix/tarball/master")
    { }).ghcide-ghc865;
in mkDerivation {
  pname = "dummy";
  version = "";
  src = "";
  libraryHaskellDepends = [
    cabal-install
    #apply-refact
    hdevtools
    #zmcat
    #hindent
    #fswatch
    hlint
    #protolude
    #ghcide
    fix-imports
    optparse-applicative
    shake
    Cabal
    Glob
    ghcid
    dhall-json
    #typed-process
    #unix
  ];
  description = "";
  license = stdenv.lib.licenses.mit;
}
