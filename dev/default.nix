{ src ? ../.

, nixpkgs ?
  builtins.fetchTarball "http://nixos.org/channels/nixos-20.03/nixexprs.tar.xz"

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
    inherit resources;
    pythonPackages = python3Packages;
    src = src + "/pynrm";
    hsnrm = haskellPackages.hsnrm;
  };

  nrm = pkgs.symlinkJoin {
    name = "nrmFull";
    paths = [
      haskellPackages.hsnrm
      pynrm
      resources
      pkgs.linuxPackages.perf
      pkgs.hwloc
    ];
  };

  hsnrm-hack = pkgs.haskellPackages.shellFor {
    packages = p: [
      haskellPackages.hsnrm
      (haskellPackages.callPackage ./pkgs/hs-tools { inherit useGhcide; })
    ];
    withHoogle = true;
    buildInputs = [ pkgs.git pkgs.hwloc pkgs.htop pkgs.jq ];
  };

  pynrm-hack = pynrm.overrideAttrs (o: {
    propagatedBuildInputs =
      (pkgs.lib.lists.remove haskellPackages.hsnrm o.propagatedBuildInputs);
    buildInputs = with python3Packages;
      (pkgs.lib.lists.remove haskellPackages.hsnrm o.buildInputs) ++ [
        flake8
        autopep8
        black
        #mypy
        #pytype
        nbformat
        nbconvert
        pandas
        matplotlib
        #nb_black
        msgpack
        pyzmq
        warlock
        seaborn
      ];

    shellHook = ''
      export LOCALE_ARCHIVE=${pkgs.glibcLocales}/lib/locale/locale-archive
      export LANG=en_US.UTF-8
    '';
  });

  libnrm-hack = libnrm.overrideAttrs
    (o: { buildInputs = o.buildInputs ++ [ pkgs.clang-tools ]; });

  jupyterWithBatteries = (pkgs.jupyter.override rec {
    python3 = (python3Packages.python.withPackages
      (ps: with ps; [ nb_black msgpack warlock pyzmq pandas seaborn ]));
    definitions = {
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
      Rdf = {
        displayName = "R";
        argv = [
          "${
            pkgs.rWrapper.override {
              packages = with pkgs.rPackages; [
                tidyr
                purrr
                ggthemes
                ggplot2
                huxtable
                formatR
                RcppRoll
                latex2exp
                plotly
                phantomjs
                webshot
                pracma
                knitr
                JuniperKernel
              ];
            }
          }/bin/R"
          "--slave"
          "-e"
          "JuniperKernel::bootKernel()"
          "--args"
          "{connection_file}"
        ];
        language = "Rlang";
        logo32 = "${python3.sitePackages}/ipykernel/resources/logo-32x32.png";
        logo64 = "${python3.sitePackages}/ipykernel/resources/logo-64x64.png";
      };
    };
  }).overrideAttrs (_: { doCheck = false; });

  hack = pkgs.mkShell {
    inputsFrom = with pkgs; [ pynrm-hack hsnrm-hack libnrm-hack ];
    buildInputs =
      [ pkgs.hwloc haskellPackages.dhrun pkgs.which pkgs.jq pkgs.yq ];
    shellHook = ''
      # path for NRM dev experimentation
      export PYNRMSO=${
      # export for locating the client-side shared lib
      # (used by python lib nrm.tooling)
        builtins.toPath ../.
      }/.build/build/x86_64-linux/ghc-8.6.5/hsnrm-1.0.0/x/pynrm.so/build/pynrm.so/pynrm.so
      export NRMSO=${
      #export for locating the server-side shared lib
      # (used by `nrmd`)
        builtins.toPath ../.
      }/.build/build/x86_64-linux/ghc-8.6.5/hsnrm-1.0.0/x/nrm.so/build/nrm.so/nrm.so
      export PATH=${builtins.toPath ../.}/dev/:${
      #export for locating the client and server binaries
      # (`nrmd`, `nrm-perfwrapper`)
        builtins.toPath ../.
      }/pynrm/bin:${
      #export for locating the client binary
      # (`nrm`)
        builtins.toPath ../.
      }/.build/build/x86_64-linux/ghc-8.6.5/hsnrm-1.0.0/x/nrm/build/nrm:$PATH
      # export for locating the nrm python libraries
      # (`nrm.<module>`)
      export PYTHONPATH=${builtins.toPath ../.}/pynrm/:$PYTHONPATH
      # exports for `ghcide` use:
      export NIX_GHC="${haskellPackages.hsnrm.env.NIX_GHC}"
      export NIX_GHCPKG="${haskellPackages.hsnrm.env.NIX_GHCPKG}"
      export NIX_GHC_DOCDIR="${haskellPackages.hsnrm.env.NIX_GHC_DOCDIR}"
      export NIX_GHC_LIBDIR="${haskellPackages.hsnrm.env.NIX_GHC_LIBDIR}"
    '';
    LC_ALL = "en_US.UTF-8";
    LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
  };

  hack-with-devtools =
    hack.overrideAttrs (o: { buildInputs = o.buildInputs ++ [ ormolu ]; });

  myRPackages = with pkgs.rPackages; [
    tidyr
    purrr
    ggthemes
    ggplot2
    huxtable
    plotly
    formatR
    RcppRoll
    latex2exp
    plotly
    phantomjs
    webshot
    pracma
    knitr
    JuniperKernel
  ];

  expe = hack-with-devtools.overrideAttrs (o: {
    buildInputs = o.buildInputs ++ [
      pkgs.phantomjs
      pkgs.pandoc
      pkgs.daemonize
      #jupyterWithBatteries
      pkgs.texlive.combined.scheme-full
      (pkgs.rstudioWrapper.override { packages = myRPackages; })
      (pkgs.rWrapper.override { packages = myRPackages; })
    ];
    shellHook = o.shellHook + ''
      export JUPYTER_PATH=$JUPYTER_PATH:${builtins.toPath ../.}/pynrm/
    '';
  });

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

  stream = callPackage ./pkgs/stream {
    iterationCount = "400";
    inherit libnrm;
    nrmSupport = false;
  };

  amg = callPackage ./pkgs/amg {
    inherit libnrm;
    nrmSupport = false;
  };

  testGeneric = doDhrun genericTestName;
  doDhrunApp = app: doDhrun "${app} True < NoCap = {=} | Cap : Text >";
  testSTREAM = addBI (doDhrunApp "stream") stream;
}
