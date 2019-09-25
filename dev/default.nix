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
      && (baseNameOf path != ".ghc.environment.x86_64-linux-8.4.4")
      && (baseNameOf path != "result") && (baseNameOf path != "README")
      && (baseNameOf path != "dist")) path;
  };
  nrmPythonPackages = pkgs.python37Packages.override {
    overrides = self: super: rec {
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
in rec {
  hsnrm = import ./pkgs/hsnrm {
    inherit pkgs;
    inherit hslib;
    src = ../hsnrm;
  };
  pynrm = pkgs.callPackage ./pkgs/pynrm {
    pythonPackages = nrmPythonPackages;
    src = ../pynrm;
  };
  libnrm = pkgs.callPackage ./pkgs/libnrm {
    src = ../libnrm;
  };
  hsnrm-hack = pkgs.haskellPackages.shellFor {
    packages = p: [
      hsnrm
      (pkgs.haskellPackages.callPackage ./pkgs/hs-tools { })
    ];
    withHoogle = true;
    buildInputs = [ pkgs.git pkgs.hwloc pkgs.htop pkgs.jq ]
      ++ hsnrm.buildInputs;
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
    inputsFrom = with pkgs; [ hsnrm-hack libnrm-hack ];

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
