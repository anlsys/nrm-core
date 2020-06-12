with import ./. { };

haskellPackages.shellFor {
  packages = p: [ haskellPackages.hbandit ];
  withHoogle = true;
  buildInputs = [
    (rWrapper.override {
      packages = with rPackages; [ ggplot2 svglite dplyr msgpackR knitr ];
    })
    ghcid
    dhall
    pythonPackages.nbconvert
    haskellPackages.dhall-to-cabal
    haskellPackages.panpipe
    haskellPackages.panhandle
    cabal2nix
    ormolu
    hlint
    pandoc
    cabal-install
  ];
  shellHook = ''
    export R_LIBS_SITE=${builtins.readFile r-libs-site}
    export LOCALE_ARCHIVE=${glibcLocales}/lib/locale/locale-archive
    export LANG=en_US.UTF-8
    export NIX_GHC="${haskellPackages.hbandit.env.NIX_GHC}"
    export NIX_GHCPKG="${haskellPackages.hbandit.env.NIX_GHCPKG}"
    export NIX_GHC_DOCDIR="${haskellPackages.hbandit.env.NIX_GHC_DOCDIR}"
    export NIX_GHC_LIBDIR="${haskellPackages.hbandit.env.NIX_GHC_LIBDIR}"
  '';
}

