{ pkgs ? import ./. { }

, libnrm-hack ? true, hsnrm-hack ? true, pynrm-hack ? true

, experiment ? false, analysis ? false, ihaskell ? false

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
    repr
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
      packages = p: [ haskellPackages.hsnrm haskellPackages.hsnrm-extra ];
      withHoogle = true;
      buildInputs = [
        pkgs.git
        pkgs.pythonPackages.mypy
        pkgs.hwloc
        pkgs.htop
        pkgs.jq
        pkgs.procps
        haskellPackages.cabal-install
        haskellPackages.graphmod
        haskellPackages.hdevtools
        haskellPackages.wreq
        haskellPackages.hlint
        haskellPackages.fix-imports
        haskellPackages.optparse-applicative
        haskellPackages.shake
        haskellPackages.Cabal
        haskellPackages.Glob
        haskellPackages.ghcid
        haskellPackages.shelltestrunner
        haskellPackages.dhall-json
        haskellPackages.cabal2nix
      ];
    })

    ++ lib.optional libnrm-hack (libnrm.overrideAttrs
      (o: { buildInputs = o.buildInputs ++ [ pkgs.clang-tools ]; }));
  buildInputs = [ hwloc which jq yq ] ++ lib.optionals analysis [
    texlive.combined.scheme-full
    (rWrapper.override { packages = myRPackages; })

  ] ++ lib.optional ihaskell [ pkgs.ihaskell ] ++ lib.optionals experiment [
    pandoc
    pythonPackages.pandas
    pythonPackages.matplotlib
    pythonPackages.seaborn
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
  shellHook = let pwd = builtins.toPath ./.;
  in ''
    export PYNRMSO=${pwd}/hsnrm/bin/pynrm.so
    export NRMSO=${pwd}/hsnrm/bin/nrm.so
    export PATH=:${pwd}/pynrm/bin:${pwd}/hsnrm/bin:$PATH
    export PYTHONPATH=${pwd}/pynrm/:$PYTHONPATH
    export GHCRTSPATH="${haskellPackages.ghc}/lib/ghc-$(ghc --numeric-version)/rts/"
    export NIX_GHCPKG="${haskellPackages.hsnrm.env.NIX_GHCPKG}"
    export NIX_GHC_DOCDIR="${haskellPackages.hsnrm.env.NIX_GHC_DOCDIR}"
    export NIX_GHC_LIBDIR="${haskellPackages.hsnrm.env.NIX_GHC_LIBDIR}"
  '' + lib.optionalString experiment ''
    export JUPYTER_PATH=$JUPYTER_PATH:${pwd}/pynrm/
  '';
  LC_ALL = "en_US.UTF-8";
  LOCALE_ARCHIVE = "${glibcLocales}/lib/locale/locale-archive";
}
