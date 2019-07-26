{ pkgs ? import ./nix { }, }: rec {

  hnrm = pkgs.hnrm;

  hnrm-static = hnrm.overrideAttrs (old: {
    enableSharedExecutables = false;
    enableSharedLibraries = false;
    configureFlags = [
      "--ghc-option=-optl=-static"
      "--ghc-option=-optl=-pthread"
      "--ghc-option=-optl=-L${
        pkgs.pkgs.gmp6.override { withStatic = true; }
      }/lib"
      "--ghc-option=-optl=-L${pkgs.pkgs.zlib.static}/lib"
      "--ghc-option=-optl=-L${pkgs.pkgs.glibc.static}/lib"
    ];
  });

  hack = pkgs.lib.getHackEnv pkgs.pkgs pkgs pkgs.haskellPackages hnrm;

  #for Musl use pkgsMusl and:
  #enableSharedExecutables = false;
  #enableSharedLibraries = false;
  #configureFlags = [
  #"--ghc-option=-optl=-static"
  #"--extra-lib-dirs=${pkgs.gmp6.override { withStatic = true; }}/lib"
  #"--extra-lib-dirs=${pkgs.zlib.static}/lib"
  #"--extra-lib-dirs=${pkgs.libffi.overrideAttrs (old: { dontDisableStatic = true; doCheck=false; })}/lib"
  #];

}
