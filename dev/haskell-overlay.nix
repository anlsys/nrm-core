{ src }:
_: pkgs:
let
  fetched = s: (pkgs.nix-update-source.fetch s).src;
  overrides = self: super:
    with pkgs.haskell.lib;
    let
      dhall-haskell-src = pkgs.fetchurl {
        url = "https://github.com/freuk/dhall-haskell/archive/master.tar.gz";
        sha256 = "13gk3g7nivwgrsksjhvq0i0zq9ajsbrbqq2f5g95v74l6v5b7yvr";
      };
    in rec {
      hsnrm = self.callPackage (../hsnrm/hsnrm.nix) { };

      hbandit = self.callPackage (./pkgs/hbandit) {
        src = pkgs.fetchurl {
          url =
            "https://xgitlab.cels.anl.gov/argo/hbandit/-/archive/master/hbandit-master.tar.gz";
          sha256 = "05dcwnfmn01q953rrrpadfx7ax3ppxkzvcx2y701wpyfjarsbqmv";
        };
      };

      dhrun = ((self.callCabal2nix "dhrun" (builtins.fetchGit {
        inherit (pkgs.stdenv.lib.importJSON ./pkgs/dhrun/pin.json) url rev;
      })) { }).overrideAttrs (_: { doCheck = false; });

      regex = doJailbreak super.regex;
      json-schema = dontCheck (unmarkBroken (doJailbreak super.json-schema));
      zeromq4-conduit = unmarkBroken (dontCheck super.zeromq4-conduit);
      refined = unmarkBroken super.refined;
      dhall-to-cabal = unmarkBroken super.dhall-to-cabal;
    };

in { haskellPackages = pkgs.haskellPackages.override { inherit overrides; }; }
