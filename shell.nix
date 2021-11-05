{ pkgs ? import ./. { } }:
with pkgs;
haskellPackages.shellFor {
      packages = p: [ haskellPackages.hsnrm haskellPackages.hsnrm-extra ];
      withHoogle = true;
      buildInputs = [
        pkgs.hwloc
        pkgs.pkg-config
        pkgs.zeromq
        haskellPackages.cabal-install
        haskellPackages.wreq
        haskellPackages.hlint
        haskellPackages.fix-imports
        haskellPackages.optparse-applicative
        haskellPackages.shake
        haskellPackages.Cabal
        haskellPackages.Glob
        haskellPackages.ghcid
        haskellPackages.shelltestrunner
        haskellPackages.dhall-json
        haskellPackages.cabal2nix
        haskellPackages.cabal-plan
      ];
}
