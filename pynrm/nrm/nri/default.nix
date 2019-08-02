{
  pkgs ? import <argopkgs> {nri-src=./.;},
}:
rec {
  nri = pkgs.nri;
  hack = pkgs.nri-hack;
}
