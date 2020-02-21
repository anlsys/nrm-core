{ haskell, lib, mkDerivation, stdenv, cabal-install, apply-refact, cabal2nix
, wreq, hdevtools, Glob, hindent, fswatch, hlint, protolude, shake, Cabal
, graphmod, fix-imports, ghcid, typed-process, optparse-applicative, unix
, cabal-helper, dhall-json, dhall-to-cabal, useGhcide }:
let
  ghcide = (import (builtins.fetchTarball
    "https://github.com/cachix/ghcide-nix/tarball/master") { }).ghcide-ghc865;
in mkDerivation {
  pname = "dummy";
  version = "";
  src = "";
  libraryHaskellDepends = [
    cabal-install
    graphmod
    hdevtools
    dhall-to-cabal
    wreq
    hlint
    fix-imports
    optparse-applicative
    shake
    Cabal
    Glob
    ghcid
    dhall-json
    cabal2nix
  ] ++ (lib.optional useGhcide ghcide);
  #ghcide;
  description = "";
  license = stdenv.lib.licenses.mit;
}
