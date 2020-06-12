{ nixpkgs ? (builtins.fetchTarball
  "https://github.com/NixOS/nixpkgs/archive/20.03.tar.gz") }:
let

  pkgs = import nixpkgs {

    config = {
      ihaskell = {
        packages = ps:
          with ps; [
            ihaskell-charts
            ihaskell-widgets
            ad
            Chart
            hbandit
            Chart-diagrams
            lens
            protolude
            generic-lens
            generic-data
            refined
            command-qq
            probability
            xls
            intervals
            neat-interpolation
            statistics
            random-fu
            pretty-simple
          ];
      };
    };

    overlays = [
      (_: pkgs: {
        haskellPackages = pkgs.haskell.packages.ghc865.override {
          overrides = self: super:
            with pkgs.haskell.lib; rec {
              ihaskell = unmarkBroken super.ihaskell;
              vinyl = doJailbreak (unmarkBroken super.vinyl);
              ihaskell-blaze = unmarkBroken super.ihaskell-blaze;
              ihaskell-charts = unmarkBroken super.ihaskell-charts;
              ihaskell-widgets = unmarkBroken super.ihaskell-widgets;
              ihaskell-diagrams = unmarkBroken super.ihaskell-diagrams;
              ihaskell-display = unmarkBroken super.ihaskell-display;
              hbandit = self.callPackage ./hbandit.nix { };
              panpipe = unmarkBroken (doJailbreak super.panpipe);
              refined = unmarkBroken super.refined;
              dhall-to-cabal = unmarkBroken super.dhall-to-cabal;
              lazysmallcheck2012 = null;
              panhandle = doJailbreak (dontCheck (self.callCabal2nix "panhandle"
                (builtins.fetchTarball
                  "https://github.com/freuk/panhandle/archive/master.tar.gz")
                { }));
            };
        };
      })

    ];

  };

  ormolu = let
    source = pkgs.fetchFromGitHub {
      owner = "tweag";
      repo = "ormolu";
      rev = "f83f6fd1dab5ccbbdf55ee1653b24595c1d653c2";
      sha256 = "1hs7ayq5d15m9kxwfmdac3p2i3s6b0cn58cm4rrqc4d447yl426y";
    };
  in (import source { }).ormolu;

in with pkgs;
pkgs // rec {

  dhall-to-cabal-resources = pkgs.stdenv.mkDerivation {
    name = "dhall-to-cabal-resources";
    src = pkgs.haskellPackages.dhall-to-cabal.src;
    installPhase = "cp -r dhall $out";
  };

  hlint = haskellPackages.hlint;
  hbandit = haskellPackages.hbandit;

  ihaskell = pkgs.stdenv.mkDerivation {
    name = "my-jupyter";
    src = null;
    buildInputs = [ pkgs.ihaskell ];
  };

  r-libs-site = pkgs.runCommand "r-libs-site" {
    buildInputs = with pkgs; [
      R
      rPackages.ggplot2
      rPackages.svglite
      rPackages.plotly
      rPackages.latex2exp
    ];
  } "echo $R_LIBS_SITE > $out";
}
