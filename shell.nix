{ useGhcide ? false, }: (import ./dev/default.nix { inherit useGhcide; }).hack
