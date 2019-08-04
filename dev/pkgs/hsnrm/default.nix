{ pkgs, hslib, src }:
let
  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super:
      with pkgs.haskell.lib; rec {
        regex = pkgs.haskell.lib.doJailbreak super.regex;
        json-schema = pkgs.haskell.lib.doJailbreak super.json-schema;
        hsnrm = (self.callCabal2nix "hsnrm" (hslib.filter src)) { };
      };
  };
in haskellPackages.hsnrm
