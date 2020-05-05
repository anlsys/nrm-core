{ src }:
_: pkgs:
let
  fetched = s: (pkgs.nix-update-source.fetch s).src;
  unbreak = x:
    x.overrideAttrs (attrs: { meta = attrs.meta // { broken = false; }; });
  overrides = self: super:
    with pkgs.haskell.lib; rec {
      regex = doJailbreak super.regex;
      json-schema = unbreak (doJailbreak super.json-schema);
      zeromq4-conduit = unbreak (dontCheck super.zeromq4-conduit);

      hsnrm = self.callPackage (../hsnrm/hsnrm.nix) { };

      hbandit = self.callPackage (./pkgs/hbandit) {
        src = pkgs.fetchurl {
          url =
            "https://xgitlab.cels.anl.gov/argo/hbandit/-/archive/master/hbandit-master.tar.gz";
          sha256 = "05dcwnfmn01q953rrrpadfx7ax3ppxkzvcx2y701wpyfjarsbqmv";
        };
      };

      dhall = super.dhall_1_24_0;
      dhall-json = (self.callCabal2nix "dhall-json"
        (src + "/hsnrm/dhall-haskell/dhall-json") { }).overrideAttrs
        (o: { doCheck = false; });
      dhrun = ((self.callCabal2nix "dhrun" (builtins.fetchGit {
        inherit (pkgs.stdenv.lib.importJSON ./pkgs/dhrun/pin.json) url rev;
      })) { }).overrideAttrs (_: { doCheck = false; });
    };

in { haskellPackages = pkgs.haskellPackages.override { inherit overrides; }; }
