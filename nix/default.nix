{ src ? ../.

, nixpkgs ? (builtins.fetchTarball
  "https://github.com/NixOS/nixpkgs/archive/20.03.tar.gz")

}:

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
            Chart-diagrams
            lens
            protolude
            generic-lens
            generic-data
            refined
            command-qq
            probability
            intervals
            pretty-simple
          ];
      };
    };

    overlays = [
      (_: pkgs: {

        cabal2nix = pkgs.haskell.packages.ghc865.cabal2nix;

      })
      (_: pkgs: {
        dhall-to-cabal =
          pkgs.haskell.lib.unmarkBroken pkgs.haskellPackages.dhall-to-cabal;
        haskellPackages = pkgs.haskell.packages.ghc865.override {
          overrides = self: super:
            with pkgs.haskell.lib; rec {
              hsnrm = super.callCabal2nix "hsnrm" ../hsnrm/hsnrm { };
              hsnrm-extra =
                super.callCabal2nix "hsnrm-extra" ../hsnrm/hsnrm-extra { };
              hsnrm-bin =
                super.callCabal2nix "hsnrm-bin" ../hsnrm/hsnrm-bin { };
              hbandit = self.callPackage ./pkgs/hbandit { };
              regex = doJailbreak super.regex;
              json-schema =
                dontCheck (unmarkBroken (doJailbreak super.json-schema));
              zeromq4-conduit = unmarkBroken (dontCheck super.zeromq4-conduit);
              prettyprinter = super.prettyprinter_1_6_0;
              refined = doJailbreak (unmarkBroken super.refined);
              aeson-extra = unmarkBroken super.aeson-extra;
              generic-aeson = unmarkBroken super.generic-aeson;
              Plot-ho-matic = unmarkBroken super.Plot-ho-matic;
              zeromq4-haskell = unmarkBroken super.zeromq4-haskell;
              time-parsers = unmarkBroken super.time-parsers;
              dhall = super.dhall_1_29_0;
              dhall-json = doJailbreak super.dhall-json_1_6_1;
              ihaskell = unmarkBroken super.ihaskell;
              vinyl = doJailbreak (unmarkBroken super.vinyl);
              ihaskell-blaze = unmarkBroken super.ihaskell-blaze;
              ihaskell-charts = unmarkBroken super.ihaskell-charts;
              ihaskell-widgets = unmarkBroken super.ihaskell-widgets;
              ihaskell-inline-r = unmarkBroken super.ihaskell-inline-r;
              ihaskell-diagrams = unmarkBroken super.ihaskell-diagrams;
              ihaskell-display = unmarkBroken super.ihaskell-display;

            };
        };
      })
      (_: pkgs:
        let
          noCheck = p: p.overridePythonAttrs (_: { doCheck = false; });
          noCheckAll = pkgs.lib.mapAttrs (name: p: noCheck p);
          packageOverrides = pself: psuper:
            {
              pynrm = pself.callPackage ./pkgs/pynrm {
                src = src + "/pynrm";
                hsnrm = pkgs.haskellPackages.hsnrm-bin;
              };
            } // noCheckAll {
              pyzmq = psuper.pyzmq.override { zeromq = pkgs.zeromq; };
              nbconvert = psuper.nbconvert;
            };
        in rec {
          python = pkgs.python3.override
            (old: { packageOverrides = packageOverrides; });
          pythonPackages = python.pkgs;
        })
    ];
  };

in with pkgs;
pkgs // rec {

  dhall-to-cabal-resources = pkgs.stdenv.mkDerivation {
    name = "dhall-to-cabal-resources";
    src = pkgs.haskellPackages.dhall-to-cabal.src;
    installPhase = "cp -r dhall $out";
  };

  ormolu = let
    source = pkgs.fetchFromGitHub {
      owner = "tweag";
      repo = "ormolu";
      rev = "f83f6fd1dab5ccbbdf55ee1653b24595c1d653c2";
      sha256 = "1hs7ayq5d15m9kxwfmdac3p2i3s6b0cn58cm4rrqc4d447yl426y";
    };
  in (import source { }).ormolu;

  libnrm = pkgs.callPackage ./pkgs/libnrm { src = src + "/libnrm"; };

  pynrm = pkgs.callPackage ./pkgs/pynrm {
    src = src + "/pynrm";
    hsnrm = haskellPackages.hsnrm-bin;
  };

  nrm = pkgs.symlinkJoin {
    name = "nrmFull";
    paths = [
      haskellPackages.hsnrm
      haskellPackages.hsnrm-bin
      pynrm
      pkgs.linuxPackages.perf
      pkgs.hwloc
    ];
  };

  stream = callPackage ./pkgs/stream {
    iterationCount = "400";
    inherit libnrm;
    nrmSupport = false;
  };

  amg = callPackage ./pkgs/amg {
    inherit libnrm;
    nrmSupport = false;
  };
}
