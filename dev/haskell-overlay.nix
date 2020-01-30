_: pkgs:
let
  fetched = s: (pkgs.nix-update-source.fetch s).src;

  unbreak = x:
    x.overrideAttrs (attrs: { meta = attrs.meta // { broken = false; }; });

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
      GHCVERSION=$(${pkgs.haskellPackages.ghc}/bin/ghc --numeric-version)
      ${pkgs.haskellPackages.dhall-to-cabal}/bin/dhall-to-cabal <<< "./${dhallFileName} \"${pkgs.haskellPackages.ghc}\" \"$GHCVERSION\"" --output-stdout > $out
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

in {
  haskellPackages = pkgs.haskellPackages.override  { inherit overrides;};
}
