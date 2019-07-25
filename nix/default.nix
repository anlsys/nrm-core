{ # host package set (unused aside for fetching nixpkgs)
  hostPkgs         ? import <nixpkgs> {}
, # versioned nixpkgs
  pkgs             ? import (hostPkgs.nix-update-source.fetch ./pkgs.json).src {}
, # source path for hnrm
  hnrm-src        ? ../.
}:
let
  callPackage = pkgs.lib.callPackageWith (pkgs // hnrmpkgs);

  hnrmpkgs = rec {
    inherit pkgs;
    lib       = import ./utils.nix;
    haskellPackages = pkgs.haskellPackages.override {
      overrides = self: super: with pkgs.haskell.lib;
      rec {
        hnrm = (self.callCabal2nix "hnrm" (lib.filter hnrm-src) ) {};
      };
    };
    hnrm = haskellPackages.hnrm;
  };
in hnrmpkgs
