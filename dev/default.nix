{ src ? ../.

, nixpkgs ? (builtins.fetchTarball
  "https://github.com/NixOS/nixpkgs/archive/20.03.tar.gz")

, pkgs ? import ./nixpkgs.nix {
  inherit nixpkgs;
  inherit src;
}

, useGhcide ? false

}:

let callPackage = pkgs.lib.callPackageWith pkgs;

in with pkgs;
pkgs // rec {

  dhall-to-cabal-resources = pkgs.stdenv.mkDerivation {
    name = "dhall-to-cabal-resources";
    src = pkgs.haskellPackages.dhall-to-cabal.src;
    installPhase = "cp -r dhall $out";
  };

  ormolu = let
    source = pkgs.fetchFromGitHub {
      owner = "tweag";
      repo = "ormolu";
      rev = "f83f6fd1dab5ccbbdf55ee1653b24595c1d653c2";
      sha256 = "1hs7ayq5d15m9kxwfmdac3p2i3s6b0cn58cm4rrqc4d447yl426y";
    };
  in (import source { }).ormolu;

  libnrm = pkgs.callPackage ./pkgs/libnrm { src = src + "/libnrm"; };

  pynrm = pkgs.callPackage ./pkgs/pynrm {
    src = src + "/pynrm";
    hsnrm = haskellPackages.hsnrm-bin;
  };

  nrm = pkgs.symlinkJoin {
    name = "nrmFull";
    paths = [
      haskellPackages.hsnrm
      haskellPackages.hsnrm-bin
      pynrm
      pkgs.linuxPackages.perf
      pkgs.hwloc
    ];
  };

  dhrunTestConfigLayer = let src' = src;
  in pkgs.stdenv.mkDerivation rec {
    name = "dhrunSpecs";
    src = src' + "./dhrun";
    installPhase = ''
      mkdir -p $out
      cp -r $src/* $out
      substituteInPlace $out/assets/simple-H2O.xml --replace \
        H2O.HF.wfs.xml $out/assets/H2O.HF.wfs.xml
      substituteInPlace $out/assets/simple-H2O.xml --replace \
        O.BFD.xml $out/assets/O.BFD.xml
      substituteInPlace $out/assets/simple-H2O.xml --replace \
        H.BFD.xml $out/assets/H.BFD.xml
      substituteInPlace $out/lib.dh --replace \
        "dataDir = \"./\"" "dataDir = \"$out/\""
      substituteInPlace $out/lib.dh --replace \
        "https://xgitlab.cels.anl.gov/argo/dhrun/raw/master/" "./"
      ln -s ${dhrun}/share/resources $out/resources
      ln -s ${dhall-to-cabal-resources} dev/cabal/dhall-to-cabal
    '';
    unpackPhase = "true";
  };

  stream = callPackage ./pkgs/stream {
    iterationCount = "400";
    inherit libnrm;
    nrmSupport = false;
  };

  amg = callPackage ./pkgs/amg {
    inherit libnrm;
    nrmSupport = false;
  };
}
