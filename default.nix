{
  pkgs ? import ./nix {},
}:
rec {

  hnrm = pkgs.hnrm;

  hnrm-static = pkgs.hnrm.overrideAttrs (old:{
    enableSharedExecutables = false;
    enableSharedLibraries = false;
    configureFlags = [
      "--ghc-option=-optl=-static"
      "--ghc-option=-optl=-pthread"
      "--ghc-option=-optl=-L${pkgs.pkgs.gmp6.override { withStatic = true; }}/lib"
      "--ghc-option=-optl=-L${pkgs.pkgs.zlib.static}/lib"
      "--ghc-option=-optl=-L${pkgs.pkgs.glibc.static}/lib"
    ];
  });

  hack = pkgs.lib.getHackEnv pkgs.pkgs pkgs pkgs.haskellPackages hnrm;

}
