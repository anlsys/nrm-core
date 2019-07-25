{
  pkgs ? import ./nix {},
}:
rec {
  hnrm = pkgs.hnrm;
  hack = pkgs.lib.getHackEnv pkgs.pkgs pkgs pkgs.haskellPackages hnrm;
}
