{ nixpkgs ? (builtins.fetchTarball
  "https://github.com/NixOS/nixpkgs/archive/20.03.tar.gz") }:
let

  pkgs = import nixpkgs {

    overlays = [
      (_: pkgs: {
        haskellPackages = pkgs.haskell.packages.ghc865.override {
          overrides = self: super:
            with pkgs.haskell.lib; rec {
              vinyl = doJailbreak (unmarkBroken super.vinyl);
              hbandit = self.callCabal2nix "hbandit" ./. { };
              panpipe = unmarkBroken (doJailbreak super.panpipe);
              refined = unmarkBroken super.refined;
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

in with pkgs; pkgs // rec { inherit ormolu; }
