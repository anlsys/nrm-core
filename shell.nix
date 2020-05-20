{ pkgs ? import ./. { }

, libnrm-hack ? true, hsnrm-hack ? true, pynrm-hack ? true

, experiment ? false, analysis ? false

, jupyter ? false

}:

with pkgs;
let

  myRPackages = with rPackages; [
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
  ];

in mkShell {
  inputsFrom =

    lib.optional pynrm-hack (pythonPackages.pynrm.overrideAttrs (o: {
      propagatedBuildInputs = (pkgs.lib.lists.remove haskellPackages.hsnrm-bin
        o.propagatedBuildInputs);
      buildInputs = with pythonPackages;
        (pkgs.lib.lists.remove haskellPackages.hsnrm-bin o.buildInputs)
        ++ [ flake8 autopep8 black nbformat nbconvert ];
      shellHook = ''
        export LOCALE_ARCHIVE=${pkgs.glibcLocales}/lib/locale/locale-archive
        export LANG=en_US.UTF-8
      '';
    })) ++

    lib.optional hsnrm-hack (pkgs.haskellPackages.shellFor {
      packages = p: [ haskellPackages.hsnrm ];
      withHoogle = true;
      buildInputs = [
        pkgs.git
        pkgs.hwloc
        pkgs.htop
        pkgs.jq
        haskellPackages.cabal-install
        haskellPackages.graphmod
        haskellPackages.hdevtools
        pkgs.dhall-to-cabal
        haskellPackages.wreq
        haskellPackages.hlint
        haskellPackages.fix-imports
        haskellPackages.optparse-applicative
        haskellPackages.shake
        haskellPackages.Cabal
        haskellPackages.Glob
        haskellPackages.ghcid
        haskellPackages.dhall-json
        haskellPackages.cabal2nix
      ];
    })

    ++ lib.optional libnrm-hack (libnrm.overrideAttrs
      (o: { buildInputs = o.buildInputs ++ [ pkgs.clang-tools ]; }));
  buildInputs = [ hwloc dhrun which jq yq ]
    ++ lib.optionals analysis [ texlive.combined.scheme-full ]
    ++ lib.optionals experiment [
      pandoc
      daemonize
      pythonPackages.pandas
      pythonPackages.matplotlib
      pythonPackages.seaborn
      (rWrapper.override { packages = myRPackages; })
    ] ++ lib.optional jupyter ((pkgs.jupyter.override rec {
      python3 = (pythonPackages.python.withPackages
        (ps: with ps; [ msgpack warlock pyzmq pandas seaborn nbformat ]));
      definitions = {
        python = {
          displayName = "Python";
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
    }).overrideAttrs (_: { doCheck = false; }));
  shellHook = ''
    # path for NRM dev experimentation
    export PYNRMSO=${
    # export for locating the client-side shared lib
    # (used by python lib nrm.tooling)
      builtins.toPath ./.
    }/hsnrm/dist-newstyle/build/x86_64-linux/ghc-8.6.5/hsnrm-bin-1.0.0/x/pynrm.so/build/pynrm.so/pynrm.so
    export NRMSO=${
    #export for locating the server-side shared lib
    # (used by `nrmd`)
      builtins.toPath ./.
    }/hsnrm/dist-newstyle/build/x86_64-linux/ghc-8.6.5/hsnrm-bin-1.0.0/x/nrm.so/build/nrm.so/nrm.so
    export PATH=${builtins.toPath ./.}/dev/:${
    #export for locating the client and server binaries
    # (`nrmd`, `nrm-perfwrapper`)
      builtins.toPath ./.
    }/pynrm/bin:${
    #export for locating the client binary
    # (`nrm`)
      builtins.toPath ./.
    }/.build/build/x86_64-linux/ghc-8.6.5/hsnrm-1.0.0/x/nrm/build/nrm:$PATH
    # export for locating the nrm python libraries
    # (`nrm.<module>`)
    export PYTHONPATH=${builtins.toPath ./.}/pynrm/:$PYTHONPATH
    # exports for `ghcide` use:
    export NIX_GHC="${haskellPackages.hsnrm.env.NIX_GHC}"
    export NIX_GHCPKG="${haskellPackages.hsnrm.env.NIX_GHCPKG}"
    export NIX_GHC_DOCDIR="${haskellPackages.hsnrm.env.NIX_GHC_DOCDIR}"
    export NIX_GHC_LIBDIR="${haskellPackages.hsnrm.env.NIX_GHC_LIBDIR}"
  '' + lib.optionalString experiment ''
    export JUPYTER_PATH=$JUPYTER_PATH:${builtins.toPath ./.}/pynrm/
  '';
  LC_ALL = "en_US.UTF-8";
  LOCALE_ARCHIVE = "${glibcLocales}/lib/locale/locale-archive";
}
