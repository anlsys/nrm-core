# this file was tested using GNUMAKE >= 4.2.1.

# this is necessary for using multi-line strings as command arguments.
SHELL := $(shell which bash)

# this allows omitting newlines.
.ONESHELL:

# "nix-shell -p" constructs an expression that relies on <nixpkgs> for
# selecting attributes, so we override it.
# https://github.com/NixOS/nix/issues/726#issuecomment-161215255
NIX_PATH := nixpkgs=./.

all: bin/nrm bin/nrm.so bin/pynrm.so

.PHONY: bin/nrm
bin/nrm.so bin/nrm: hsnrm-extra/hsnrm-extra.cabal hsnrm-static/hsnrm-static.cabal hsnrm-bin/hsnrm-bin.cabal hsnrm/hsnrm.cabal
	@nix-shell --pure --run bash <<< '
		cabal v2-install hsnrm-bin --installdir=./bin --overwrite-policy=always
	'

.PHONY: bin/pynrm.so
bin/pynrm.so: hsnrm-extra/hsnrm-extra.cabal hsnrm-static/hsnrm-static.cabal hsnrm-bin/hsnrm-bin.cabal hsnrm/hsnrm.cabal
	ln -s $$GHCRTSPATH hsnrm-extra/lHSrts
	@nix-shell --pure --run bash <<< '
		cabal v2-install hsnrm-extra:pynrm.so --installdir=./bin --overwrite-policy=always
	'

.PHONY: ghcid
ghcid: ghcid-hsnrm

ghcid-%: .hlint.yaml %/*.cabal
	ghcid --command "cabal v2-repl $* " \
		--restart=default.nix \
		-l

.PHONY: pre-commit
pre-commit: hsnrm/hsnrm.cabal hsnrm-static/hsnrm-static.cabal hsnrm-bin/hsnrm-bin.cabal dhall-format ormolu shellcheck hlint

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
	rm -f hsnrm-extra/lHSrts/
	rm -rf dist-newstyle
	rm -f hsnrm.cabal
	rm -f hsnrm.nix
	rm -f a.out
