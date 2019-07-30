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
  hnrmShell = pkgs.haskellPackages.shellFor {
      packages = p: [ hnrm ];
      #withHoogle = true;
      #buildInputs = [pkgs.git pkgs.hwloc pkgs.htop pkgs.jq] ++ hnrm.buildInputs;
      GHC_GMP = "${pkgs.gmp6.override { withStatic = true; }}/lib";
      GHC_ZLIB = "${pkgs.zlib.static}/lib";
      GHC_GLIBC = "${pkgs.glibc.static}/lib";
  };

  hnrm-static = pkgsStatic.hnrm.overrideAttrs (old: {
    enableSharedExecutables = false;
    enableSharedLibraries = false;
    configureFlags = [
      "--ghc-option=-lHSrts-ghc${pkgsStatic.haskellPackages.ghc.version}"
      "--ghc-option=-optl=-static"
      "--ghc-option=-optl=-pthread"
      "--ghc-option=-optl=-L${pkgs.gmp6.override { withStatic = true; }}/lib"
      "--ghc-option=-optl=-L${pkgs.zlib.static}/lib"
      "--ghc-option=-optl=-L${pkgs.glibc.static}/lib"
    ];
  });

  pySelector = p: with p; [ msgpack ];
  pyEnv = pkgs.pkgs.python3.withPackages pySelector;

  #hack =
    #(pkgs.lib.getHackEnv pkgs.pkgs pkgs pkgs.haskellPackages hnrm).overrideAttrs
    #(o: {
      #buildInputs = o.buildInputs ++ [ pyEnv ];
    #});

  hack =
    pkgs.haskellPackages.shellFor {
      packages = p: [ hnrm (pkgs.haskellPackages.callPackage hackTools { }) ];
      withHoogle = true;
      buildInputs = [pkgs.git pkgs.hwloc pkgs.htop pkgs.jq] ++ hnrm.buildInputs;
      GHC_GMP = "${pkgs.gmp6.override { withStatic = true; }}/lib";
      GHC_ZLIB = "${pkgs.zlib.static}/lib";
      GHC_GLIBC = "${pkgs.glibc.static}/lib";
    };

  hack-build =
    (pkgsStatic.lib.getHackEnv pkgsStatic pkgsStatic pkgsStatic.haskellPackages pkgsStatic.hnrm).overrideAttrs (o: {
      buildInputs = o.buildInputs ++ [ pyEnv ];
      GHC_GMP = "${pkgsStatic.gmp6.override { withStatic = true; }}/lib";
      GHC_ZLIB = "${pkgsStatic.zlib.static}/lib";
      GHC_GLIBC = "${pkgsStatic.glibc.static}/lib";
    });

}
