{
  pkgs ? import ./nix {},
}:
rec {
  hnrm = pkgs.hnrm;
  hack = pkgs.lib.getHackEnv pkgs.bleeding pkgs pkgs.bleedingHaskellPackages hnrm;
}
