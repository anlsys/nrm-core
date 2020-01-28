{ hostPkgs ? import <nixpkgs> { }

, fetched ? s: (hostPkgs.nix-update-source.fetch s).src

, pkgs ? import (fetched ./pkgs.json) { }

, useGhcide ? false

}:

let
  hslib = rec {
    filter = path:
      builtins.filterSource (path: _:
        (baseNameOf path != ".hdevtools.sock") && (baseNameOf path != ".ghc.*")
        && (baseNameOf path != "result") && (baseNameOf path != "README")
        && (baseNameOf path != "dist")) path;
  };
  unbreak = x:
    x.overrideAttrs (attrs: { meta = attrs.meta // { broken = false; }; });
  callPackage = pkgs.lib.callPackageWith pkgs;

in with pkgs;
pkgs // rec {

  noCheck = p : p.overridePythonAttrs (_:{doCheck=false;});
  noCheckAll = lib.mapAttrs (name : p : noCheck p);

  python = let
    packageOverrides = pself: psuper: (noCheckAll {
      cffi = psuper.cffi.overridePythonAttrs (o: { doCheck = false; });
      sqlalchemy =
        psuper.sqlalchemy.overridePythonAttrs (o: { doCheck = false; });
      requests = psuper.requests.overridePythonAttrs (o: { doCheck = false; });
      sphinx = psuper.sphinx.overridePythonAttrs (o: { doCheck = false; });
      cryptography =
        psuper.cryptography.overridePythonAttrs (o: { doCheck = false; });
      cython = psuper.cython.overridePythonAttrs (o: { doCheck = false; });
      hypothesis =
        psuper.hypothesis.overridePythonAttrs (o: { doCheck = false; });
      black = psuper.black.overridePythonAttrs (o: { doCheck = false; });
      pytest = psuper.pytest.overridePythonAttrs (o: { doCheck = false; });
      networkx = psuper.networkx.overridePythonAttrs (o: { doCheck = false; });
      pandas = psuper.pandas.overridePythonAttrs (o: { doCheck = false; });
      ssl = psuper.ssl.overridePythonAttrs (o: { doCheck = false; });
      pySSL = psuper.pySSL.overridePythonAttrs (o: { doCheck = false; });
      seaborn = psuper.seaborn.overridePythonAttrs (o: { doCheck = false; });
      importlab = pkgs.callPackage ./pkgs/importlab { pythonPackages = pself; };
      pyzmq = psuper.pyzmq.override { zeromq = pkgs.zeromq; };
      pytype = pkgs.callPackage ./pkgs/pytype {
        src = fetched ./pkgs/pytype/pin.json;
        pythonPackages = pself;
      };
      nb_black = pkgs.callPackage ./pkgs/nb_black {
        src = pkgs.fetchFromGitHub {
          owner = "dnanhkhoa";
          repo = "nb_black";
          rev = "cf4a07f83ab4fbfa2a2728fdb8a0605704c830dd";
          sha256 = "11qapvda8jk8pagbk7nipr137jm58i68nr45yar8qg8p3cvanjzf";
        };
        pythonPackages = pself;
      };
    });
  in pkgs.python37.override {
    inherit packageOverrides;
    self = python;
  };
  pythonPackages = python.passthru.pkgs;

  dhall-to-cabal-resources = pkgs.stdenv.mkDerivation {
    name = "dhall-to-cabal-resources";
    src = pkgs.haskellPackages.dhall-to-cabal.src;
    installPhase = "cp -r dhall $out";
  };

  #this needs to be seriously cleaned.
  cabalFile = dhallDir: dhallFileName:
    pkgs.runCommand "cabalFile" { } ''
      export LOCALE_ARCHIVE=${pkgs.glibcLocales}/lib/locale/locale-archive
      export LANG=en_US.UTF-8
      cp ${dhallDir}/* .
      substituteInPlace common.dhall --replace "= ./dhall-to-cabal" "= ${dhall-to-cabal-resources}"
      substituteInPlace ${dhallFileName} --replace "= ./dhall-to-cabal" "= ${dhall-to-cabal-resources}"
      GHCVERSION=$(${haskellPackages.ghc}/bin/ghc --numeric-version)
      ${haskellPackages.dhall-to-cabal}/bin/dhall-to-cabal <<< "./${dhallFileName} \"${haskellPackages.ghc}\" \"$GHCVERSION\"" --output-stdout > $out
    '';

  patchedSrc = source: rename: dhallDir: dhallFileName:
    pkgs.runCommand "patchedSrc" { } ''
      mkdir -p $out
      cp -r ${source}/${rename} $out/${rename}
      cp -r ${source}/hbandit $out/hbandit
      cp -r ${source}/glpk $out/glpk
      chmod -R +rw $out
      cp ${cabalFile dhallDir dhallFileName} $out/hsnrm.cabal
    '';

  patchedSrcLib = patchedSrc ../hsnrm "nrm" ./cabal "lib.dhall";

  patchedSrcBin = patchedSrc ../hsnrm "bin" ./cabal "bin.dhall";

  ormolu = let
    source = pkgs.fetchFromGitHub {
      owner = "tweag";
      repo = "ormolu";
      rev = "f83f6fd1dab5ccbbdf55ee1653b24595c1d653c2";
      sha256 = "1hs7ayq5d15m9kxwfmdac3p2i3s6b0cn58cm4rrqc4d447yl426y";
    };
  in (import source { }).ormolu;
  #ormolu = (import (builtins.fetchTarball
  #"https://github.com/tweag/ormolu/archive/0.0.1.0.tar.gz") { }).ormolu;

  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super:
      with pkgs.haskell.lib; rec {
        regex = doJailbreak super.regex;
        json-schema = unbreak (doJailbreak super.json-schema);
        zeromq4-conduit = unbreak (dontCheck super.zeromq4-conduit);
        nrmlib =
          (self.callCabal2nix "hsnrm.cabal" patchedSrcLib { }).overrideAttrs
          (o: { nativeBuildInputs = o.nativeBuildInputs ++ [ pkgs.glpk ]; });
        nrmbin = (self.callCabal2nix "hsnrm.cabal" patchedSrcBin {
          inherit nrmlib;
        }).overrideAttrs
          (o: { nativeBuildInputs = o.nativeBuildInputs ++ [ pkgs.glpk ]; });
        dhall = super.dhall_1_24_0;
        dhall-json =
          (self.callCabal2nix "dhall-json" ../hsnrm/dhall-haskell/dhall-json
            { }).overrideAttrs (o: { doCheck = false; });
        dhrun = (self.callCabal2nix "dhrun" (builtins.fetchGit {
          inherit (pkgs.stdenv.lib.importJSON ./pkgs/dhrun/pin.json) url rev;
        })) { };
      };
  };

  nrmso-nodoc = pkgs.haskell.lib.dontHaddock haskellPackages.nrmlib;

  resources = pkgs.runCommand "patchedSrc" { } ''
    mkdir -p $out/share/nrm
    ${haskellPackages.nrmbin}/bin/codegen $out/share/nrm/
  '';

  libnrm = pkgs.callPackage ./pkgs/libnrm {
    inherit resources;
    src = ../libnrm;
    hsnrm = haskellPackages.nrmbin;
  };

  pynrm = pkgs.callPackage ./pkgs/pynrm {
    inherit resources;
    pythonPackages = pythonPackages;
    src = ../pynrm;
    hsnrm = haskellPackages.nrmbin;
  };

  nrm = pkgs.symlinkJoin {
    name = "nrmFull";
    paths = [
      haskellPackages.nrmbin
      pynrm
      resources
      pkgs.linuxPackages.perf
      pkgs.hwloc
    ];
  };

  hsnrm-hack = pkgs.haskellPackages.shellFor {
    packages = p: [
      haskellPackages.nrmlib
      (haskellPackages.callPackage ./pkgs/hs-tools { inherit useGhcide; })
    ];
    withHoogle = true;
    buildInputs = [ pkgs.git pkgs.hwloc pkgs.htop pkgs.jq ];
  };

  pynrm-hack = pynrm.overrideAttrs (o: {
    propagatedBuildInputs =
      (pkgs.lib.lists.remove haskellPackages.nrmbin o.propagatedBuildInputs);
    buildInputs = with pythonPackages;
      (pkgs.lib.lists.remove haskellPackages.nrmbin o.buildInputs)
      ++ [ flake8 autopep8 black mypy pytype nbformat nbconvert nb_black ];

    shellHook = ''
      export LOCALE_ARCHIVE=${pkgs.glibcLocales}/lib/locale/locale-archive
      export LANG=en_US.UTF-8
    '';
  });

  libnrm-hack = libnrm.overrideAttrs
    (o: { buildInputs = o.buildInputs ++ [ pkgs.clang-tools ]; });

  #jupyter = import /home/fre/sandbox/jupyterWith { };

  jupyter = import (pkgs.fetchFromGitHub {
    owner = "freuk";
    repo = "jupyterWith";
    rev = "9fc066c6a3b91d6ea462b822ca5909985e5748fc";
    sha256 = "0ii8rm09bz2mhrn2z7q20gjx27y3dqzldp1zkfz64860ml31lb12";
  }) { };

  jupyterLabEnvironment = let
    lab = (jupyter.jupyterlabWith {
      extraInputsFrom = _: [ hsnrm-hack libnrm-hack ];
      extraPackages = _: [
        pkgs.hwloc
        ormolu
        haskellPackages.dhrun
        pkgs.daemonize
        pkgs.linuxPackages.perf
      ];
      extraJupyterPath = "${builtins.toPath ../.}/pynrm/:${
          pythonPackages.makePythonPath (with pythonPackages; [
            nb_black
            msgpack
            warlock
            pyzmq
            pandas
            seaborn
          ])
        }:";
      kernels = [
        (import ./jupyterWith/ipython.nix {
          inherit (pkgs) stdenv writeScriptBin;
          name = "Nix";
          packages = p: [
            p.nb_black
            p.msgpack
            p.warlock
            p.pyzmq
            p.pandas
            p.seaborn
          ];
          python3 = python;
        })
      ];
    }).env;
  in lab.overrideAttrs (o: {
    shellHook = o.shellHook + ''
      # path for NRM dev experimentation
      export PYNRMSO=${
        builtins.toPath ../.
      }/.build/build/x86_64-linux/ghc-8.6.5/hsnrm-1.0.0/x/pynrm.so/build/pynrm.so/pynrm.so
      export NRMSO=${
        builtins.toPath ../.
      }/.build/build/x86_64-linux/ghc-8.6.5/hsnrm-1.0.0/x/nrm.so/build/nrm.so/nrm.so
      export PATH=${builtins.toPath ../.}/dev/:${
        builtins.toPath ../.
      }/pynrm/bin:${
        builtins.toPath ../.
      }/.build/build/x86_64-linux/ghc-8.6.5/hsnrm-1.0.0/x/nrm/build/nrm:$PATH
      export PYTHONPATH=${builtins.toPath ../.}/pynrm/:${
        pythonPackages.makePythonPath [ ]
      }:$PYTHONPATH
      export JUPYTER_PATH=$JUPYTER_PATH:${builtins.toPath ../.}/pynrm/
      # exports for `ghcide` use
      export NIX_GHC="${haskellPackages.nrmlib.env.NIX_GHC}"
      export NIX_GHCPKG="${haskellPackages.nrmlib.env.NIX_GHCPKG}"
      export NIX_GHC_DOCDIR="${haskellPackages.nrmlib.env.NIX_GHC_DOCDIR}"
      export NIX_GHC_LIBDIR="${haskellPackages.nrmlib.env.NIX_GHC_LIBDIR}"
      export LC_ALL="en_US.UTF-8";
      export LOCALE_ARCHIVE="${pkgs.glibcLocales}/lib/locale/locale-archive";
      export CABALFILE=${
        cabalFile ./cabal "dev.dhall"
      } # for easy manual vendoring
      cp $CABALFILE hsnrm/hsnrm.cabal
      chmod +rw hsnrm/hsnrm.cabal
    '';
  });

  hack = pkgs.mkShell {
    CABALFILE = cabalFile ./cabal "dev.dhall"; # for easy manual vendoring
    inputsFrom = with pkgs; [ pynrm-hack hsnrm-hack libnrm-hack ];
    buildInputs = [
      pkgs.hwloc
      ormolu
      haskellPackages.dhrun
      #jupyterWithBatteries
      pkgs.daemonize
    ];
    shellHook = ''
      # path for NRM dev experimentation
      export PYNRMSO=${
        builtins.toPath ../.
      }/.build/build/x86_64-linux/ghc-8.6.5/hsnrm-1.0.0/x/pynrm.so/build/pynrm.so/pynrm.so
      export NRMSO=${
        builtins.toPath ../.
      }/.build/build/x86_64-linux/ghc-8.6.5/hsnrm-1.0.0/x/nrm.so/build/nrm.so/nrm.so
      export PATH=${builtins.toPath ../.}/dev/:${
        builtins.toPath ../.
      }/pynrm/bin:${
        builtins.toPath ../.
      }/.build/build/x86_64-linux/ghc-8.6.5/hsnrm-1.0.0/x/nrm/build/nrm:$PATH
      export PYTHONPATH=${builtins.toPath ../.}/pynrm/:$PYTHONPATH
      export JUPYTER_PATH=$JUPYTER_PATH:${builtins.toPath ../.}/pynrm/
      # exports for `ghcide` use
      export NIX_GHC="${haskellPackages.nrmlib.env.NIX_GHC}"
      export NIX_GHCPKG="${haskellPackages.nrmlib.env.NIX_GHCPKG}"
      export NIX_GHC_DOCDIR="${haskellPackages.nrmlib.env.NIX_GHC_DOCDIR}"
      export NIX_GHC_LIBDIR="${haskellPackages.nrmlib.env.NIX_GHC_LIBDIR}"
      cp $CABALFILE hsnrm/hsnrm.cabal
      chmod +rw hsnrm/hsnrm.cabal
    '';
    LC_ALL = "en_US.UTF-8";
    LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
  };

  dhrun = haskellPackages.dhrun.overrideAttrs (old: {
    installPhase = old.installPhase + ''
      mkdir -p $out/share/
      cp -r resources $out/share/
    '';
  });

  dhrunTestConfigLayer = pkgs.stdenv.mkDerivation rec {
    name = "dhrunSpecs";
    src = ./dhrun;
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

  doDhrun = dhallcall:
    test.overrideAttrs (old: {
      buildPhase = ''
        dhrun run <<< 'let all = ${dhrunTestConfigLayer}/all-tests.dh
                      "${dhrunTestConfigLayer}/" "${nrm}/share/examples/" in all.${dhallcall}'
      '';
      installPhase = ''
        mkdir -p $out
        cp _output/* $out/
      '';
    });

  stream-raw = callPackage ./pkgs/stream {
    iterationCount = "2000";
    inherit libnrm;
    nrmSupport = false;
  };

  stream = callPackage ./pkgs/stream {
    iterationCount = "2000";
    inherit libnrm;
    nrmSupport = true;
  };

  testGeneric = doDhrun genericTestName;
  doDhrunApp = app: doDhrun "${app} True < NoCap = {=} | Cap : Text >";
  testSTREAM = addBI (doDhrunApp "stream") stream;
}
