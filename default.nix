{ hostPkgs ? import <nixpkgs> { }
  # versioned nixpkgs
, bleedingPkgs ?
  import (hostPkgs.nix-update-source.fetch ./nix/pkgs.json).src { }

, staticPkgs ? import (builtins.fetchTarball
  "https://github.com/NixOS/nixpkgs-channels/archive/08d245eb31a3de0ad73719372190ce84c1bf3aee.tar.gz")
  { }

}:
let
  pkgs = import ./nix { pkgs = bleedingPkgs; };
  pkgsStatic = import ./nix { pkgs = staticPkgs; };
  hackTools =
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
    };
in rec {

  hnrm = pkgs.hnrm;

  pySelector = p: with p; [ msgpack ];
  pyEnv = pkgs.pkgs.python3.withPackages pySelector;

  hack = pkgs.haskellPackages.shellFor {
    packages = p: [ hnrm (pkgs.haskellPackages.callPackage hackTools { }) ];
    withHoogle = true;
    buildInputs = [ pkgs.git pkgs.hwloc pkgs.htop pkgs.jq pyEnv ] ++ hnrm.buildInputs;
    GHC_GMP = "${pkgs.gmp6.override { withStatic = true; }}/lib";
    GHC_ZLIB = "${pkgs.zlib.static}/lib";
    GHC_GLIBC = "${pkgs.glibc.static}/lib";
    GHC_FFI =
      "${pkgs.libffi.overrideAttrs (old: { dontDisableStatic = true; })}/lib";
    GHC_VERSION = "${pkgs.haskellPackages.ghc.version}";
  };

}
