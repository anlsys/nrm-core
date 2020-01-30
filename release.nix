{ hnrm ? ./. }:
let

  pkgs = import hnrm { src = hnrm; };

in {
  inherit (pkgs.haskellPackages)
    nrmbin nrmlib libnrm resources pynrm jupyterWithBatteries dhrun;
}
