{ pkgs ? import (builtins.fetchTarball
  "http://nixos.org/channels/nixos-20.03/nixexprs.tar.xz") { } }:
with pkgs.lib;

let
  ormolu = let
    source = pkgs.fetchFromGitHub {
      owner = "tweag";
      repo = "ormolu";
      rev = "f83f6fd1dab5ccbbdf55ee1653b24595c1d653c2";
      sha256 = "1hs7ayq5d15m9kxwfmdac3p2i3s6b0cn58cm4rrqc4d447yl426y";
    };
  in (import source { }).ormolu;

in pkgs // rec {

  dhall-to-cabal-resources = pkgs.stdenv.mkDerivation {
    name = "dhall-to-cabal-resources";
    src = pkgs.haskellPackages.dhall-to-cabal.src;
    installPhase = "cp -r dhall $out";
  };

  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super:
      with pkgs.haskell.lib; rec {
        hbandit = self.callPackage ./hbandit.nix {};
        panpipe = doJailbreak super.panpipe;
        refined = unmarkBroken super.refined;
        lazysmallcheck2012 = null;
        panhandle = doJailbreak (dontCheck (self.callCabal2nix "panhandle"
          (builtins.fetchTarball
            "https://github.com/freuk/panhandle/archive/master.tar.gz") { }));
      };
  };
  inherit ormolu;

  hlint = haskellPackages.hlint;
  hbandit = haskellPackages.hbandit;

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
