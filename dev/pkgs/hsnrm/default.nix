{ pkgs, hslib, src }:
let
  unbreak = x:
    x.overrideAttrs (attrs: { meta = attrs.meta // { broken = false; }; });
  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super:
      with pkgs.haskell.lib; rec {
        regex = pkgs.haskell.lib.doJailbreak super.regex;
        json-schema = unbreak (pkgs.haskell.lib.doJailbreak super.json-schema);
        zeromq4-conduit =
          unbreak (pkgs.haskell.lib.dontCheck super.zeromq4-conduit);
        hsnrm = (self.callCabal2nix "hsnrm" (hslib.filter src)) { };
        dhall = super.dhall_1_24_0;
      };
  };
in haskellPackages.hsnrm
