{ pkgs, hslib, src }:
let
  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super:
      with pkgs.haskell.lib; rec {
        regex = pkgs.haskell.lib.doJailbreak super.regex;
        json-schema =
          (pkgs.haskell.lib.doJailbreak super.json-schema).overrideAttrs
          (attrs: { meta = attrs.meta // { broken = false; }; });
        zeromq4-conduit = pkgs.haskell.lib.dontCheck super.zeromq4-conduit;
        hsnrm = (self.callCabal2nix "hsnrm" (hslib.filter src)) { };
        dhall = super.dhall_1_24_0;
      };
  };
in haskellPackages.hsnrm
