{ hostPkgs ? import <nixpkgs> { }

, fetched ? s: (hostPkgs.nix-update-source.fetch s).src

, pkgs ? import (fetched ./pkgs.json) { }

, staticPkgs ? import (builtins.fetchTarball
  "https://github.com/NixOS/nixpkgs-channels/archive/08d245eb31a3de0ad73719372190ce84c1bf3aee.tar.gz")
  { }

}:
let
  hslib = rec {
    filter = path:
      builtins.filterSource (path: _:
      (baseNameOf path != ".hdevtools.sock")
      && (baseNameOf path != ".ghc.*")
      && (baseNameOf path != "result") && (baseNameOf path != "README")
      && (baseNameOf path != "dist")) path;
  };
  nrmPythonPackages = pkgs.python37Packages.override {
    overrides = self: super: rec {
      cffi = super.cffi.overridePythonAttrs (o: { doCheck = false; });
      sqlalchemy =
        super.sqlalchemy.overridePythonAttrs (o: { doCheck = false; });
      requests = super.requests.overridePythonAttrs (o: { doCheck = false; });
      sphinx = super.sphinx.overridePythonAttrs (o: { doCheck = false; });
      cryptography =
        super.cryptography.overridePythonAttrs (o: { doCheck = false; });
      cython = super.cython.overridePythonAttrs (o: { doCheck = false; });
      hypothesis =
        super.hypothesis.overridePythonAttrs (o: { doCheck = false; });
      black = super.black.overridePythonAttrs (o: { doCheck = false; });
      pytest = super.pytest.overridePythonAttrs (o: { doCheck = false; });
      networkx = super.networkx.overridePythonAttrs (o: { doCheck = false; });
      importlab = pkgs.callPackage ./pkgs/importlab { pythonPackages = self; };
      pyzmq = super.pyzmq.override { zeromq = pkgs.zeromq; };
      pytype = pkgs.callPackage ./pkgs/pytype {
        src = fetched ./pkgs/pytype/pin.json;
        pythonPackages = self;
      };
    };
  };
  unbreak = x:
    x.overrideAttrs (attrs: { meta = attrs.meta // { broken = false; }; });
in rec {
  dhall-to-cabal = (haskellPackages.callCabal2nix "dhall-to-cabal"
    (builtins.fetchTarball
    "https://github.com/dhall-lang/dhall-to-cabal/archive/1.3.4.0.tar.gz")) { };

  cabalFile = dhallSpec:
    pkgs.runCommand "cabalFile" { } ''
      export LOCALE_ARCHIVE=${pkgs.glibcLocales}/lib/locale/locale-archive
      export LANG=en_US.UTF-8
      cp ${dhallSpec} cabal.dhall
      substituteInPlace cabal.dhall --replace "= ./dhall" "= ${./cabal/dhall}"
      GHCVERSION=$(${haskellPackages.ghc}/bin/ghc --numeric-version)
      ${dhall-to-cabal}/bin/dhall-to-cabal <<< "./cabal.dhall \"${haskellPackages.ghc}\" \"$GHCVERSION\"" --output-stdout > $out
    '';

  patchedSrc = source: rename: dhallFile:
    pkgs.runCommand "patchedSrc" { } ''
      mkdir -p $out
      cp -r ${source}/ $out/${rename}
      chmod -R +rw $out
      cp ${cabalFile dhallFile} $out/hsnrm.cabal
    '';

  patchedSrcLib = patchedSrc ../hsnrm/nrm "nrm" cabal/lib.dhall;

  patchedSrcBin = patchedSrc ../hsnrm/bin "bin" cabal/bin.dhall;

  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super:
      with pkgs.haskell.lib; rec {
        regex = doJailbreak super.regex;
        json-schema = unbreak (doJailbreak super.json-schema);
        zeromq4-conduit = unbreak (dontCheck super.zeromq4-conduit);
        nrmlib = self.callCabal2nix "hsnrm.cabal" patchedSrcLib { };
        nrmbin =
          self.callCabal2nix "hsnrm.cabal" patchedSrcBin { inherit nrmlib; };
        dhall = super.dhall_1_24_0;
      };
  };

  resources = pkgs.runCommand "patchedSrc" { } ''
    mkdir -p $out/share/
    ${haskellPackages.nrmbin}/bin/codegen a $out/share/
  '';

  libnrm = pkgs.callPackage ./pkgs/libnrm {
    src = ../libnrm;
    #resources = haskellPackages.resources;
    hsnrm = haskellPackages.nrmbin;
  };

  pynrm = pkgs.callPackage ./pkgs/pynrm {
    inherit resources;
    pythonPackages = nrmPythonPackages;
    src = ../pynrm;
    hsnrm = haskellPackages.nrmbin;
  };

  nrm = pkgs.symlinkJoin {
    name = "nrmFull";
    paths = [ haskellPackages.nrmbin pynrm resources pkgs.linuxPackages.perf];
  };

  hsnrm-hack = pkgs.haskellPackages.shellFor {
    packages = p: [
      haskellPackages.nrmlib
      (pkgs.haskellPackages.callPackage ./pkgs/hs-tools { })
    ];
    withHoogle = true;
    buildInputs = [ pkgs.git pkgs.hwloc pkgs.htop pkgs.jq ]
      ++ haskellPackages.nrmlib.buildInputs;
  };
  pynrm-hack = pynrm.overrideAttrs (o: {
    buildInputs = o.buildInputs ++ [
      nrmPythonPackages.flake8
      nrmPythonPackages.autopep8
      nrmPythonPackages.black
      nrmPythonPackages.mypy
      nrmPythonPackages.pytype
      nrmPythonPackages.sphinx
    ];
  });
  libnrm-hack = libnrm.overrideAttrs
    (o: { buildInputs = o.buildInputs ++ [ pkgs.astyle ]; });

  hack = pkgs.mkShell {

    CABALFILE = cabalFile cabal/dev.dhall; # for easy manual vendoring

    inputsFrom = with pkgs; [ pynrm-hack hsnrm-hack libnrm-hack ];

    buildInputs = [ pkgs.hwloc ];

    shellHook = ''
      export PATH=$PATH:./dev/:./pynrm/bin:./hsnrm/dist-newstyle/build/x86_64-linux/ghc-8.6.5/hsnrm-1.0.0/x/nrm/build/nrm/
      export PYTHONPATH=$PYTHONPATH:./pynrm/
    '';
  };
}

#GHC_GMP = "${pkgs.gmp6.override { withStatic = true; }}/lib";
#GHC_ZLIB = "${pkgs.zlib.static}/lib";
#GHC_GLIBC = "${pkgs.glibc.static}/lib";
#GHC_FFI =
#"${pkgs.libffi.overrideAttrs (old: { dontDisableStatic = true; })}/lib";
#GHC_VERSION = "${pkgs.haskellPackages.ghc.version}";
