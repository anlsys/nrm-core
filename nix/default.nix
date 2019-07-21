{ # host package set (unused aside for fetching nixpkgs)
  hostPkgs         ? import <nixpkgs> {}
, # versioned nixpkgs
  pkgs             ? import (hostPkgs.nix-update-source.fetch ./pkgs.json).src {}
, # versioned nixpkgs-older version for zmcat
  bleeding         ? import (hostPkgs.nix-update-source.fetch ./bleeding.json).src {}
, # fetcher alias for the remaining arguments
  fetched          ? s: (pkgs.nix-update-source.fetch s).src
  # source path for hnrm
, hnrm-src        ? ../.
}:
let
  callPackage = pkgs.lib.callPackageWith (pkgs // hnrmpkgs);

  hnrmpkgs = rec {
    inherit bleeding;
    lib       = import ./utils.nix;
    haskellPackages = pkgs.haskellPackages.override {
      overrides = self: super: with pkgs.haskell.lib;
      rec {
      };
    };
    bleedingHaskellPackages = bleeding.haskellPackages.override {
      overrides = self: super: with bleeding.haskell.lib;
      rec {
        hnrm = (self.callCabal2nix "hnrm" (lib.filter hnrm-src) ) {};
      };
    };
    hnrm = bleedingHaskellPackages.hnrm;
  };
in hnrmpkgs
