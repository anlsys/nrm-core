{ nixpkgs ? import ./nix {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, directory, hxt, hxt-xpath
      , pretty-simple, protolude, refined, stdenv, typed-process
      }:
      mkDerivation {
        pname = "hnrm";
        version = "1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        libraryHaskellDepends = [
          base directory hxt hxt-xpath pretty-simple protolude refined
          typed-process
        ];
        executableHaskellDepends = [ base protolude ];
        doHaddock = false;
        description = "hnrm configuration";
        license = stdenv.lib.licenses.bsd3;
enableSharedExecutables = false;
enableSharedLibraries = false;
configureFlags = [
  "--ghc-option=-optl=-static"
  "--ghc-option=-optl=-pthread"
  "--ghc-option=-optl=-L${pkgs.gmp6.override { withStatic = true; }}/lib"
  "--ghc-option=-optl=-L${pkgs.zlib.static}/lib"
  "--ghc-option=-optl=-L${pkgs.glibc.static}/lib"
];
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
