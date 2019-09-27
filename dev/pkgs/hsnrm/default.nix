{ pkgs, hslib, src, target }:
let
  unbreak = x:
    x.overrideAttrs (attrs: { meta = attrs.meta // { broken = false; }; });
  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super:
      with pkgs.haskell.lib; rec {
        regex = doJailbreak super.regex;
        json-schema = unbreak (doJailbreak super.json-schema);
        zeromq4-conduit = unbreak (dontCheck super.zeromq4-conduit);
        hsnrm = (self.callCabal2nix target (hslib.filter src)) { };
        dhall = super.dhall_1_24_0;
      };
  };
in haskellPackages.hsnrm
