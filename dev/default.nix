{ src ? ../.

, nixpkgs ? <nixpkgs>

, pkgs ? import ./nixpkgs.nix {
  inherit nixpkgs;
  inherit src;
}

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
  callPackage = pkgs.lib.callPackageWith pkgs;

in with pkgs;
pkgs // rec {

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

  ormolu = let
    source = pkgs.fetchFromGitHub {
      owner = "tweag";
      repo = "ormolu";
      rev = "f83f6fd1dab5ccbbdf55ee1653b24595c1d653c2";
      sha256 = "1hs7ayq5d15m9kxwfmdac3p2i3s6b0cn58cm4rrqc4d447yl426y";
    };
  in (import source { }).ormolu;

  nrmso-nodoc = pkgs.haskell.lib.dontHaddock haskellPackages.nrmlib;

  resources = pkgs.runCommand "patchedSrc" { } ''
    mkdir -p $out/share/nrm
    ${haskellPackages.nrmbin}/bin/codegen $out/share/nrm/
  '';

  libnrm = pkgs.callPackage ./pkgs/libnrm {
    inherit resources;
    src = src + "../libnrm";
    hsnrm = haskellPackages.nrmbin;
  };

  pynrm = pkgs.callPackage ./pkgs/pynrm {
    inherit resources;
    pythonPackages = python37Packages;
    src = src + "../pynrm";
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
    buildInputs = with python37Packages;
      (pkgs.lib.lists.remove haskellPackages.nrmbin o.buildInputs)
      ++ [ flake8 autopep8 black mypy pytype nbformat nbconvert nb_black ];

    shellHook = ''
      export LOCALE_ARCHIVE=${pkgs.glibcLocales}/lib/locale/locale-archive
      export LANG=en_US.UTF-8
    '';
  });

  libnrm-hack = libnrm.overrideAttrs
    (o: { buildInputs = o.buildInputs ++ [ pkgs.clang-tools ]; });

  jupyterWithBatteries = pkgs.jupyter.override rec {
    python3 = python37Packages.python.withPackages
      (ps: with ps; [ nb_black msgpack warlock pyzmq pandas seaborn ]);
    definitions = {
      # This is the Python kernel we have defined above.
      python3 = {
        displayName = "Python 3";
        argv = [
          "${python3.interpreter}"
          "-m"
          "ipykernel_launcher"
          "-f"
          "{connection_file}"
        ];
        language = "python";
        logo32 = "${python3.sitePackages}/ipykernel/resources/logo-32x32.png";
        logo64 = "${python3.sitePackages}/ipykernel/resources/logo-64x64.png";
      };
    };
  };

  hack = let src' = src;
  in pkgs.mkShell {
    CABALFILE = cabalFile ./cabal "dev.dhall"; # for easy manual vendoring
    CABALFILE_LIB = cabalFile ./cabal "lib.dhall"; # for easy manual vendoring
    CABALFILE_BIN = cabalFile ./cabal "bin.dhall"; # for easy manual vendoring
    NIXFILE_LIB = (haskellPackages.haskellSrc2nix {
      name = "hsnrm";
      src =
        (patchedSrc (src + "/hsnrm") "nrm" (src + "/dev/cabal") "lib.dhall");
    });
    NIXFILE_BIN = (haskellPackages.haskellSrc2nix {
      name = "hsnrm";
      src =
        (patchedSrc (src + "/hsnrm") "nrm" (src + "/dev/cabal") "bin.dhall");
    });
    inputsFrom = with pkgs; [ pynrm-hack hsnrm-hack libnrm-hack ];
    buildInputs = [
      pkgs.hwloc
      ormolu
      haskellPackages.dhrun
      jupyterWithBatteries
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
      cp $CABALFILE_LIB dev/pkgs/hnrm/lib.cabal
      cp $CABALFILE_BIN dev/pkgs/hnrm/bin.cabal
      cp $NIXFILE_LIB/default.nix dev/pkgs/hnrm/lib.nix
      cp $NIXFILE_BIN/default.nix dev/pkgs/hnrm/bin.nix
      chmod +rw hsnrm/hsnrm.cabal dev/pkgs/hnrm/bin.nix dev/pkgs/hnrm/lib.nix dev/pkgs/hnrm/bin.cabal dev/pkgs/hnrm/lib.cabal
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
