{ pkgs, hslib, src }:
let
  unbreak = x:
    x.overrideAttrs (attrs: { meta = attrs.meta // { broken = false; }; });
  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super:
      with pkgs.haskell.lib; rec {
        #template-haskell = super.template-haskell_2_14_0_0;
        record = addBuildDepends (addSetupDepends
          ((doJailbreak super.record).overrideAttrs (o: {
            src = (builtins.fetchTarball
              "https://github.com/nikita-volkov/record/archive/0.3.2.1.tar.gz");
          })) [ super.cabal-doctest ]) [
            super.template-haskell-compat-v0208
            super.attoparsec
            super.doctest
          ];
        regex = doJailbreak super.regex;
        json-schema = unbreak (doJailbreak super.json-schema);
        zeromq4-conduit = unbreak (dontCheck super.zeromq4-conduit);
        hsnrm = (self.callCabal2nix "hsnrm" (hslib.filter src)) { };
        dhall = super.dhall_1_24_0;
      };
  };
in haskellPackages.hsnrm
