{ pkgs, hslib, src, target }:
let
  dhall-to-cabal = (haskellPackages.callCabal2nix "dhall-to-cabal"
    (builtins.fetchTarball
    "https://github.com/dhall-lang/dhall-to-cabal/archive/1.3.4.0.tar.gz")) { };
  cabalFile = pkgs.runCommand "cabalFile" { } ''
    export LOCALE_ARCHIVE=${pkgs.glibcLocales}/lib/locale/locale-archive
    export LANG=en_US.UTF-8
    ${dhall-to-cabal}/bin/dhall-to-cabal ${src}/dhall/${target}.dhall --output-stdout > $out
  '';
  realSrc = pkgs.runCommand "realSrc" { } ''
    cp -r ${src} $out
    chmod +rw $out
    cp ${cabalFile} $out/hsnrm.cabal
  '';
  unbreak = x:
    x.overrideAttrs (attrs: { meta = attrs.meta // { broken = false; }; });
  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super:
      with pkgs.haskell.lib; rec {
        regex = doJailbreak super.regex;
        json-schema = unbreak (doJailbreak super.json-schema);
        zeromq4-conduit = unbreak (dontCheck super.zeromq4-conduit);
        hsnrm = self.callCabal2nix "hsnrm.cabal" realSrc { };
        dhall = super.dhall_1_24_0;
      };
  };
in haskellPackages.hsnrm
