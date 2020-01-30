{ haskell, lib, mkDerivation, stdenv, cabal-install, apply-refact, cabal2nix
, hdevtools, Glob, hindent, fswatch, hlint, protolude, shake, Cabal, fix-imports
, ghcid, typed-process, optparse-applicative, unix, cabal-helper, dhall-json, dhall-to-cabal
, useGhcide }:
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
    dhall-to-cabal
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
    dhall-json
    cabal2nix
    #typed-process
    #unix
  ] ++ (lib.optional useGhcide ghcide);
  #ghcide;
  description = "";
  license = stdenv.lib.licenses.mit;
}
