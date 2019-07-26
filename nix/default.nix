{ # host package set (unused aside for fetching nixpkgs)
#hostPkgs             ? import <nixpkgs> {}
# versioned nixpkgs
#,  pkgs             ? import (hostPkgs.nix-update-source.fetch ./pkgs.json).src {}
pkgs ? import (builtins.fetchTarball
  "https://github.com/NixOS/nixpkgs-channels/archive/08d245eb31a3de0ad73719372190ce84c1bf3aee.tar.gz")
  { }

, # source path for hnrm
hnrm-src ? ../. }: rec {
  inherit pkgs;
  lib = import ./utils.nix;
  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super:
      with pkgs.haskell.lib; rec {
        hnrm = (self.callCabal2nix "hnrm" (lib.filter hnrm-src)) { };
      };
  };
  hnrm = haskellPackages.hnrm;
}
