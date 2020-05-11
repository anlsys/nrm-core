# this file was tested using GNUMAKE >= 4.2.1.

# this is necessary for using multi-line strings as command arguments.
SHELL := $(shell which bash)

# this allows omitting newlines.
.ONESHELL:

# "nix-shell -p" constructs an expression that relies on <nixpkgs> for
# selecting attributes, so we override it.
# https://github.com/NixOS/nix/issues/726#issuecomment-161215255
NIX_PATH := nixpkgs=./.

all: hsnrm-static/hsnrm-static.cabal hsnrm-bin/hsnrm-bin.cabal hsnrm/hsnrm.cabal dist-newstyle/build/x86_64-linux/ghc-8.6.5/hsnrm-1.0.0/x/nrm.so/build/nrm.so/nrm.so resources

dhall-to-cabal: ../default.nix
	rm -rf ./dhall-to-cabal
	echo $$(nix-build -A dhall-to-cabal-resources --no-out-link)
	cp -r $$(nix-build -A dhall-to-cabal-resources --no-out-link) ./dhall-to-cabal
	chmod -R +rw ./dhall-to-cabal

%.cabal: %.dhall common.dhall dhall-to-cabal
	@nix-shell --pure -E '
		with import <nixpkgs> {};
		mkShell {
			buildInputs = [ haskellPackages.dhall-to-cabal haskellPackages.ghc ];
			LOCALE_ARCHIVE="$${pkgs.glibcLocales}/lib/locale/locale-archive";
			LANG="en_US.UTF-8";
			GHCDIR="$${haskellPackages.ghc}";
		}
	' --run bash <<< '
		GHCVERSION=$$(ghc --numeric-version)
		echo injecting ghc version $$GHCVERSION and ghc directory $$GHCDIR in cabal file.
		dhall-to-cabal --output-stdout <<< "./$*.dhall \"$$GHCDIR\" \"$$GHCVERSION\"" > $@
	'

dist-newstyle/build/x86_64-linux/ghc-8.6.5/hsnrm-1.0.0/x/nrm.so/build/nrm.so/nrm.so: hsnrm/ hsnrm-bin/
	@nix-shell --pure --run bash <<< '
		cabal v2-build hsnrm-bin
	'

resources: hsnrm/* hsnrm-bin/*
	@nix-shell --pure --run bash <<< '
		rm -rf resources
		mkdir resources
		cabal v2-run hsnrm-bin/Codegen.hs resources/
	'

.PHONY: ghcid
ghcid: ghcid-hsnrm

ghcid-%: .hlint.yaml %/*.cabal
	@nix-shell -E '
		with import <nixpkgs> {};
		with haskellPackages;
		shellFor {
			packages = p: [p.$*];
			buildInputs = [ghcid cabal-install hlint];
		}
	' --pure --run bash <<< '
		ghcid --command "cabal v2-repl $* " \
			--restart=hsnrm.cabal \
			--restart=default.nix \
			-l
	'

.PHONY: pre-commit
pre-commit: hsnrm/hsnrm.cabal hsnrm-static/hsnrm-static.cabal hsnrm-bin/hsnrm-bin.cabal resources dhall-format ormolu shellcheck hlint

.PHONY: dhall-format
dhall-format:
	@nix-shell --pure -p fd haskellPackages.dhall --run bash <<< '
		RETURN=0
		for F in $$(fd -e dhall); do
			dhall format < $$F | cmp -s $$F -
			if [ $$? -ne 0 ]; then
				echo "[!] $$F does not pass dhall-format format check. Formatting.." >&2
				dhall format --inplace $$F
				RETURN=1
			fi
		done
		if [ $$RETURN -ne 0 ]; then exit 1; fi
	'

.PHONY: ormolu
ormolu:
	@nix-shell --pure -E '
		let pkgs = import <nixpkgs> {};
		in pkgs.mkShell {
			buildInputs = [pkgs.fd pkgs.ormolu];
			shellHook =
				"export LOCALE_ARCHIVE=$${pkgs.glibcLocales}/lib/locale/locale-archive \n" +
				"export LANG=en_US.UTF-8";
		}
	' --run bash <<< '
		RETURN=0
		for F in $$(fd -E src/Bandit/Tutorial.hs -e hs); do
			ormolu -o -XTypeApplications -o -XPatternSynonyms -m check $$F
			if [ $$? -ne 0 ]; then
				echo "[!] $$F does not pass ormolu format check. Formatting.." >&2
				ormolu -o -XTypeApplications -o -XPatternSynonyms -m inplace $$F
				RETURN=1
			fi
		done
		if [ $$RETURN -ne 0 ]; then exit 1; fi
	'

.PHONY: shellcheck
shellcheck:
	@nix-shell --pure -p fd shellcheck --run bash <<< '
		for F in $$(fd -e sh); do
			shellcheck -s bash $$F
		done
	'

.PHONY: hlint
hlint:
	@nix-shell --pure -p hlint --run bash <<< '
		hlint hsnrm/ --hint=./.hlint.yaml
		hlint hsnrm-bin/ --hint=./.hlint.yaml
	'

.PHONY:clean
clean:
	rm -rf dist-newstyle
	rm -rf resources
	rm -f hsnrm.cabal
	rm -f hsnrm.nix
	rm -f a.out
