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
      nrmlib = (self.callPackage (./pkgs/hnrm/lib.nix) {
        src = src + "/hsnrm";
      }).overrideAttrs (o: {

        configurePhase = ''
          cp ${./pkgs/hnrm/lib.cabal} hsnrm.cabal
        '' + o.configurePhase;
      });
      nrmbin = (self.callPackage (./pkgs/hnrm/bin.nix) {
        src = src + "/hsnrm";
      }).overrideAttrs (o: {
        configurePhase = ''
          cp ${./pkgs/hnrm/bin.cabal} hsnrm.cabal
        '' + o.configurePhase;
      });
      #nrmbin = (self.callCabal2nix "hsnrm.cabal" patchedSrcBin {
      #inherit nrmlib;
      #}).overrideAttrs
      #(o: { nativeBuildInputs = o.nativeBuildInputs ++ [ pkgs.glpk ]; });
      dhall = super.dhall_1_24_0;
      dhall-json = (self.callCabal2nix "dhall-json"
        (src + "/hsnrm/dhall-haskell/dhall-json") { }).overrideAttrs
        (o: { doCheck = false; });
      dhrun = ((self.callCabal2nix "dhrun" (builtins.fetchGit {
        inherit (pkgs.stdenv.lib.importJSON ./pkgs/dhrun/pin.json) url rev;
      })) { }).overrideAttrs (_:{doCheck = false;});
    };

in { haskellPackages = pkgs.haskellPackages.override { inherit overrides; }; }
