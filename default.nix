{ pkgs ? import ./nix { }, pkgsStatic ? import ./nix {
  pkgs = import (builtins.fetchTarball
    "https://github.com/NixOS/nixpkgs-channels/archive/08d245eb31a3de0ad73719372190ce84c1bf3aee.tar.gz")
    { };
} }: rec {

  hnrm = pkgs.hnrm;

  hnrm-static = hnrm.overrideAttrs (old: {
    enableSharedExecutables = false;
    enableSharedLibraries = false;
    configureFlags = [
      "--ghc-option=-optl=-static"
      "--ghc-option=-optl=-pthread"
      "--ghc-option=-optl=-L${
        pkgs.pkgs.gmp6.override { withStatic = true; }
      }/lib"
      "--ghc-option=-optl=-L${pkgs.pkgs.zlib.static}/lib"
      "--ghc-option=-optl=-L${pkgs.pkgs.glibc.static}/lib"
    ];
  });

  pySelector = p: with p; [ msgpack ];
  pyEnv = pkgs.pkgs.python3.withPackages pySelector;

  hack =
    (pkgs.lib.getHackEnv pkgs.pkgs pkgs pkgs.haskellPackages hnrm).overrideAttrs
    (o: {
      buildInputs = o.buildInputs ++ [ pyEnv ];
      shellHook = ''
        GHC_VERSION=$(ghc --numeric-version)
      '';
    });

  #for Musl use pkgsMusl and:
  #enableSharedExecutables = false;
  #enableSharedLibraries = false;
  #configureFlags = [
  #"--ghc-option=-optl=-static"
  #"--extra-lib-dirs=${pkgs.gmp6.override { withStatic = true; }}/lib"
  #"--extra-lib-dirs=${pkgs.zlib.static}/lib"
  #"--extra-lib-dirs=${pkgs.libffi.overrideAttrs (old: { dontDisableStatic = true; doCheck=false; })}/lib"
  #];

}
