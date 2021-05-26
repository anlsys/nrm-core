{ nixpkgs ? (builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/20.03.tar.gz")
}:

let
  pkgs = import nixpkgs {

    overlays = [
      (_: pkgs: {

        cabal2nix = pkgs.haskell.packages.ghc865.cabal2nix;

      })
      (_: pkgs: {
        haskellPackages = pkgs.haskell.packages.ghc865.override {
          overrides = self: super:
            with pkgs.haskell.lib; rec {
              hsnrm = super.callCabal2nix "hsnrm" ./hsnrm { };
              hsnrm-extra =
                super.callCabal2nix "hsnrm-extra" ./hsnrm-extra { };
              hsnrm-bin =
                super.callCabal2nix "hsnrm-bin" ./hsnrm-bin { };
              hbandit = self.callPackage ./nix/hbandit.nix { };
              iso-deriving = self.callCabal2nix "iso-deriving"
                (builtins.fetchTarball
                  "https://github.com/hanshoglund/iso-deriving/archive/master.tar.gz")
                { };
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
              dhall = overrideSrc super.dhall_1_29_0 {
                version = "1.30.0";
                src = builtins.fetchTarball
                  "https://hackage.haskell.org/package/dhall-1.30.0/dhall-1.30.0.tar.gz";
              };
              dhall-json = (overrideSrc (super.dhall-json_1_6_1) {
                version = "1.6.2";
                src = builtins.fetchTarball
                  "https://hackage.haskell.org/package/dhall-json-1.6.2/dhall-json-1.6.2.tar.gz";
              }).override { inherit dhall; };
              vinyl = doJailbreak (unmarkBroken super.vinyl);

            };
        };
      })
    ];
  };

in with pkgs;
pkgs // rec {
  nrm-core = pkgs.symlinkJoin {
    name = "nrmFull";
    paths = [
      haskellPackages.hsnrm
      haskellPackages.hsnrm-bin
      haskellPackages.hsnrm-extra
    ];
  };
}
