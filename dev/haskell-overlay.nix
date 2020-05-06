{ }:
_: pkgs: {
  haskellPackages = pkgs.haskell.packages.ghc865.override {
    overrides = self: super:
      with pkgs.haskell.lib;
      let
        unmarkBroken = drv: overrideCabal drv (drv: { broken = false; });

        dhall-haskell-src = pkgs.fetchurl {
          url = "https://github.com/freuk/dhall-haskell/archive/master.tar.gz";
          sha256 = "13gk3g7nivwgrsksjhvq0i0zq9ajsbrbqq2f5g95v74l6v5b7yvr";
        };
      in rec {
        hsnrm = super.callCabal2nix "hsnrm" ../hsnrm/hsnrm { };

        hsnrm-bin = super.callCabal2nix "hsnrm-bin" ../hsnrm/hsnrm-bin { };

        hbandit = self.callPackage (./pkgs/hbandit) {
          src = pkgs.fetchurl {
            url =
              "https://xgitlab.cels.anl.gov/argo/hbandit/-/archive/master/hbandit-master.tar.gz";
            sha256 = "05dcwnfmn01q953rrrpadfx7ax3ppxkzvcx2y701wpyfjarsbqmv";
          };
        };

        dhrun = (self.callPackage (./pkgs/dhrun) {
          src = pkgs.fetchgit {
            url = "https://github.com/freuk/dhrun.git";
            rev = "929598cbc19b2aa922ede50a37d9045bc29e1adf";
            sha256 = "2GfjN60NJrr1LlohXkps35QKhDJEV3wwWvAatfRmdS0=";
          };
        }).overrideAttrs (old: {
          doCheck = false;
          installPhase = old.installPhase + ''
            mkdir -p $out/share/
            cp -r resources $out/share/
          '';
        });

        regex = doJailbreak super.regex;
        json-schema = dontCheck (unmarkBroken (doJailbreak super.json-schema));
        zeromq4-conduit = unmarkBroken (dontCheck super.zeromq4-conduit);
        refined = unmarkBroken super.refined;
        aeson-extra = unmarkBroken super.aeson-extra;
        generic-aeson = unmarkBroken super.generic-aeson;
        zeromq4-haskell = unmarkBroken super.zeromq4-haskell;
        time-parsers = unmarkBroken super.time-parsers;
        dhall-to-cabal = unmarkBroken super.dhall-to-cabal;
      };
  };
}
