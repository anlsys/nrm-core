# this file was tested using GNUMAKE >= 4.2.1.

# this is necessary for using multi-line strings as command arguments.
SHELL := $(shell which bash)

# this allows omitting newlines.
.ONESHELL:

# "nix-shell -p" constructs an expression that relies on <nixpkgs> for
# selecting attributes, so we override it.
# https://github.com/NixOS/nix/issues/726#issuecomment-161215255
NIX_PATH := nixpkgs=../.

.PHONY: vendor
vendor: hsnrm.nix

dhall-to-cabal: ../default.nix
	rm -rf ./dhall-to-cabal
	echo $$(nix-build -A dhall-to-cabal-resources --no-out-link)
	cp -r $$(nix-build -A dhall-to-cabal-resources --no-out-link) ./dhall-to-cabal
	chmod -R +rw ./dhall-to-cabal

#generating the vendored cabal file.
.PRECIOUS: hsnrm.cabal
hsnrm.cabal: hsnrm.dhall dhall-to-cabal
	@nix-shell --pure -E '
		with import <nixpkgs> {};
		mkShell {
			buildInputs = [ haskellPackages.dhall-to-cabal ];
			LOCALE_ARCHIVE="$${pkgs.glibcLocales}/lib/locale/locale-archive";
			LANG="en_US.UTF-8";
			GHCVERSION="$${haskellPackages.ghc}/bin/ghc --numeric-version)";
			GHCDIC="$${haskellPackages.ghc}";
		}
	' --run bash <<< '
		dhall-to-cabal <<< "./hsnrm.dhall \"$$GHCDIR\" \"$$GHCVERSION\""
	'

.PRECIOUS: hsnrm.nix
hsnrm.nix: hsnrm.cabal
	@nix-shell --pure -p cabal2nix --run bash <<< '
		cabal2nix . > hsnrm.nix
	'
