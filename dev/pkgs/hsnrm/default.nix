{ pkgs, hslib, src }:
let
  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super:
      with pkgs.haskell.lib; rec {
        regex = pkgs.haskell.lib.doJailbreak super.regex;
        hsnrm = (self.callCabal2nix "hsnrm" (hslib.filter src)) { };
      };
  };
in haskellPackages.hsnrm
