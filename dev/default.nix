{ src ? ../.

, nixpkgs ? (builtins.fetchTarball
  "https://github.com/NixOS/nixpkgs/archive/20.03.tar.gz")

}:

let
  pkgs = import nixpkgs {
    overlays = [
      (_: pkgs: {

        cabal2nix = pkgs.haskell.packages.ghc865.cabal2nix;

      })
      (_: pkgs: {
        dhall-to-cabal =
          pkgs.haskell.lib.unmarkBroken pkgs.haskellPackages.dhall-to-cabal;
        dhrun = pkgs.haskellPackages.callPackage ./pkgs/dhrun { };
        haskellPackages = pkgs.haskell.packages.ghc865.override {
          overrides = self: super:
            with pkgs.haskell.lib; rec {
              hsnrm = super.callCabal2nix "hsnrm" ../hsnrm/hsnrm { };
              hsnrm-bin =
                super.callCabal2nix "hsnrm-bin" ../hsnrm/hsnrm-bin { };
              hbandit = self.callPackage ./pkgs/hbandit { };

              regex = doJailbreak super.regex;
              json-schema =
                dontCheck (unmarkBroken (doJailbreak super.json-schema));
              zeromq4-conduit = unmarkBroken (dontCheck super.zeromq4-conduit);
              prettyprinter = super.prettyprinter_1_6_0;
              refined = doJailbreak (unmarkBroken super.refined);
              aeson-extra = unmarkBroken super.aeson-extra;
              generic-aeson = unmarkBroken super.generic-aeson;
              zeromq4-haskell = unmarkBroken super.zeromq4-haskell;
              time-parsers = unmarkBroken super.time-parsers;
              dhall = super.dhall_1_29_0;
              dhall-json = doJailbreak super.dhall-json_1_6_1;
            };
        };
      })
      (_: pkgs:
        let
          noCheck = p: p.overridePythonAttrs (_: { doCheck = false; });
          noCheckAll = pkgs.lib.mapAttrs (name: p: noCheck p);
          packageOverrides = pself: psuper:
            {
              pynrm = pself.callPackage ./pkgs/pynrm {
                src = src + "/pynrm";
                hsnrm = pkgs.haskellPackages.hsnrm-bin;
              };
            } // noCheckAll {
              pyzmq = psuper.pyzmq.override { zeromq = pkgs.zeromq; };
              nbconvert = psuper.nbconvert;
            };
        in rec {
          python = pkgs.python3.override (old: {
            packageOverrides =
              pkgs.lib.composeExtensions (old.packageOverrides or (_: _: { }))
              packageOverrides;
          });
          pythonPackages = python.passthru.pkgs;
        })
    ];
  };

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
