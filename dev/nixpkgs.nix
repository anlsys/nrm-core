{ src

, config ? { }

, overlays ? [ ]

, nixpkgs ? <nixpkgs>

, hostPkgs ? import nixpkgs { }

}:

let
  #fetched = s: (hostPkgs.nix-update-source.fetch s).src;
  defaultOverlays = [
    (import ./overlay.nix { })
    (import ./haskell-overlay.nix { })
    (import ./python-overlay.nix { inherit src; })
  ];
  overlaysAll = defaultOverlays ++ overlays;

in import (builtins.fetchTarball
  "https://github.com/NixOS/nixpkgs/archive/20.03.tar.gz") {
    inherit config;
    overlays = overlaysAll;
  }
