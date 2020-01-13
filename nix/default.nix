{ hostPkgs ? import <nixpkgs> { }

, fetched ? s: (hostPkgs.nix-update-source.fetch s).src

, pkgs ? import (fetched ./pin.json) { }

}:
with pkgs.lib;
let

  dhall-to-cabal-resources = pkgs.stdenv.mkDerivation {
    name = "dhall-to-cabal-resources";
    src = pkgs.haskellPackages.dhall-to-cabal.src;
    installPhase = "cp -r dhall $out";
  };

  hs-tools = { haskell, mkDerivation, stdenv, cabal-install, apply-refact
    , hdevtools, Glob, hindent, fswatch, hlint, protolude, shake, Cabal
    , fix-imports, ghcid, typed-process, optparse-applicative, unix
    , cabal-helper, dhall }:
    let
      ghcide = (import (builtins.fetchTarball
        "https://github.com/hercules-ci/ghcide-nix/tarball/master")
        { }).ghcide-ghc865;

    in mkDerivation {
      pname = "dummy";
      version = "";
      src = "";
      libraryHaskellDepends = [
        cabal-install
        dhall
        ormolu
        hlint
        ghcide
        ghcid
        shake
        Glob
        typed-process
      ];
      description = "";
      license = stdenv.lib.licenses.mit;
    };

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

  cabalFile = dhallSpec:
    pkgs.runCommand "cabalFile" { } ''
      export LOCALE_ARCHIVE=${pkgs.glibcLocales}/lib/locale/locale-archive
      export LANG=en_US.UTF-8
      cp ${dhallSpec} cabal.dhall
      substituteInPlace cabal.dhall --replace "= ./dhall-to-cabal" "= ${dhall-to-cabal-resources}"
      GHCVERSION=$(${haskellPackages.ghc}/bin/ghc --numeric-version)
      ${haskellPackages.dhall-to-cabal}/bin/dhall-to-cabal <<< "./cabal.dhall \"${haskellPackages.ghc}\" \"$GHCVERSION\"" --output-stdout > $out
    '';

  patchedSrc = source: dhallFile:
    pkgs.runCommand "patchedSrc" { } ''
      mkdir -p $out
      cp -r ${source}/* $out
      chmod -R +rw $out
      cp ${cabalFile dhallFile} $out/hbandit.cabal
      chmod +rw $out/hbandit.cabal
    '';

  ormolu = let
    source = pkgs.fetchFromGitHub {
      owner = "tweag";
      repo = "ormolu";
      rev = "f83f6fd1dab5ccbbdf55ee1653b24595c1d653c2";
      sha256 = "1hs7ayq5d15m9kxwfmdac3p2i3s6b0cn58cm4rrqc4d447yl426y";
    };
  in (import source { }).ormolu;

  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super:
      with pkgs.haskell.lib; rec {
        dhall = super.dhall_1_24_0;
        hbandit =
          self.callCabal2nix "hbandit.cabal" (patchedSrc ../. ../hbandit.dhall)
          { };
      };
  };

  pythonPackages = pkgs.python37Packages.override {
    overrides = self: super: rec {
      black = super.black.overridePythonAttrs (o: { doCheck = false; });
      SMPyBandits =
        pkgs.callPackage ./pkgs/SMPyBandits { pythonPackages = self; };
    };
  };

  jupyterWithBatteries = pkgs.jupyter.override rec {
    python3 = pythonPackages.python.withPackages
      (ps: with ps; [ msgpack pythonPackages.SMPyBandits ipywidgets ]);
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

in pkgs // rec {

  inherit haskellPackages;
  inherit pythonPackages;
  inherit ormolu;

  hbandit = haskellPackages.hbandit;

  hack = pkgs.haskellPackages.shellFor {
    packages = p: [
      haskellPackages.hbandit
      (haskellPackages.callPackage hs-tools { })
    ];
    withHoogle = true;
    buildInputs = [
      (pkgs.rWrapper.override {
        packages = with pkgs.rPackages; [ ggplot2 dplyr msgpackR knitr ];
      })
      jupyterWithBatteries
      pythonPackages.black
    ];
    shellHook = ''
      export SHELLSO=${
        builtins.toPath ../.
      }/dist-newstyle/build/x86_64-linux/ghc-8.6.5/hbandit-1.0.0/x/hbandit/build/hbandit/hbandit
      export NIX_GHC="${haskellPackages.hbandit.env.NIX_GHC}"
      export NIX_GHCPKG="${haskellPackages.hbandit.env.NIX_GHCPKG}"
      export NIX_GHC_DOCDIR="${haskellPackages.hbandit.env.NIX_GHC_DOCDIR}"
      export NIX_GHC_LIBDIR="${haskellPackages.hbandit.env.NIX_GHC_LIBDIR}"
      export LOCALE_ARCHIVE=${pkgs.glibcLocales}/lib/locale/locale-archive
      export LANG=en_US.UTF-8
      cp $CABALFILE hbandit.cabal
      ln -s ${dhall-to-cabal-resources} dhall-to-cabal
      chmod +rw hbandit.cabal
    '';
    CABALFILE = cabalFile ../hbandit.dhall;
  };
}
