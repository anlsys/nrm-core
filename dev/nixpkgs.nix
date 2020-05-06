{ src

, config ? { }

, overlays ? [ ]

, nixpkgs ? <nixpkgs>

, hostPkgs ? import nixpkgs { }

}:

let
  fetched = s: (hostPkgs.nix-update-source.fetch s).src;
  defaultOverlays = [
    (import ./python-overlay.nix { inherit src; })
    (import ./haskell-overlay.nix { })
  ];
  overlaysAll = defaultOverlays ++ overlays;

in import (builtins.fetchTarball
  "http://nixos.org/channels/nixos-20.03/nixexprs.tar.xz") {
    inherit config;
    overlays = overlaysAll;
  }
