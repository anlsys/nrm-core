{ hnrm ? ./. }:
let

  pkgs = import hnrm { src = hnrm; };

in {
  inherit (pkgs.haskellPackages)
    nrmbin nrmlib;
  inherit (pkgs)
    libnrm resources pynrm jupyterWithBatteries;
}
