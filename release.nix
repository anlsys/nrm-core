let
  pkgs = import ./pkgs/default.nix { };
  hydraPackages = { inherit (pkgs) libnrm; };
in {
  hnrmchannel = pkgs.releaseTools.channel {
    name = "hnrmchannel";
    src = ./.;
    constituents = with pkgs; [ tools.zymake ];
  };
} // hydraPackages
