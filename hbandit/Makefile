# this file was tested using GNUMAKE >= 4.2.1.

# this is necessary for using multi-line strings as command arguments.
SHELL := $(shell which bash)

# this allows omitting newlines.
.ONESHELL:

# "nix-shell -p" constructs an expression that relies on <nixpkgs> for
# selecting attributes, so we override it.
# https://github.com/NixOS/nix/issues/726#issuecomment-161215255
NIX_PATH := nixpkgs=./.

.PHONY: vendor
vendor: hbandit.nix

dhall-to-cabal: default.nix
	rm -rf ./dhall-to-cabal
	cp -r $$(nix-build -A dhall-to-cabal-resources --no-out-link) ./dhall-to-cabal
	chmod -R +rw ./dhall-to-cabal

#generating the vendored cabal file.
.PRECIOUS: hbandit.cabal
hbandit.cabal: hbandit.dhall dhall-to-cabal
	@nix-shell --pure -E '
		with import <nixpkgs> {};
		mkShell {
			buildInputs = [ haskellPackages.dhall-to-cabal ];
			LOCALE_ARCHIVE="$${pkgs.glibcLocales}/lib/locale/locale-archive";
			LANG="en_US.UTF-8";
		}
	' --run bash <<< '
		dhall-to-cabal hbandit.dhall
	'
.PRECIOUS: hbandit.nix
hbandit.nix: hbandit.cabal
	@nix-shell --pure -p cabal2nix --run bash <<< '
		cabal2nix . > hbandit.nix
	'

.PHONY: ci
ci:
	@nix-shell -p yq -p jq --run bash <<< '
		for jobname in $$(yq -r "keys| .[]" .gitlab-ci.yml); do
			if [ "$$jobname" != "stages" ]; then
				gitlab-runner exec shell "$$jobname"
			fi
		done
	'

ci-%:
	@nix-shell --run bash <<< '
		gitlab-runner exec shell "$*"
	'

.PHONY: ghcid
ghcid: ghcid-hbandit

ghcid-hbandit: hbandit.cabal .hlint.yaml hbandit.nix
	@nix-shell -E '
		with import <nixpkgs> {};
		with haskellPackages;
		shellFor {
			packages = p: [p.hbandit];
			buildInputs = [ghcid cabal-install hlint];
		}
	' --pure --run bash <<< '
		ghcid --command "cabal v2-repl hbandit " \
			--restart=hbandit.cabal \
			--restart=default.nix \
			-l
	'

ghcid-test: hbandit.cabal .hlint.yaml hbandit.nix
	@nix-shell --pure --run bash <<< '
		ghcid --command "cabal v2-repl test " \
			--restart=hbandit.cabal \
			--restart=default.nix \
			-l
	'

.PHONY: pre-commit
pre-commit: ormolu dhall-format shellcheck src/Bandit/Tutorial.hs README.md

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
		hlint src/ --hint=./.hlint.yaml
	'

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

.PHONY: doc
doc: src/Bandit/Tutorial.hs hbandit.cabal hbandit.nix
	@nix-shell -E '
		with import <nixpkgs> {};
		with haskellPackages;
		shellFor {
			packages = p: [p.hbandit];
			buildInputs = [cabal-install];
		}
	' --run <<< bash '
		cabal v2-haddock hbandit --haddock-internal
	'

.PRECIOUS: src/Bandit/Tutorial.hs
src/Bandit/Tutorial.hs: literate/tutorial.md hbandit.nix src
	@nix-shell --pure -E '
		with import <nixpkgs> {};
		with haskellPackages;
		let extra = { mkDerivation, inline-r, pretty-simple, aeson, stdenv }:
		mkDerivation {
			pname = "extra";
			version = "1.0.0";
			src = "";
			libraryHaskellDepends = [
				aeson
				inline-r
				data-default
				pretty-simple
			];
			description = "extra";
			license = stdenv.lib.licenses.bsd3;
		};
		in
		shellFor {
			packages = p: [
				p.hbandit
				(haskellPackages.callPackage extra {})
			];
			buildInputs = [
			  inline-r
				data-default
				aeson
				pretty-simple
				panhandle
				pandoc-citeproc
				panpipe
				unlit
				pandoc
				pkgs.which
				cabal-install
				R];
			R_LIBS_SITE = "$${builtins.readFile r-libs-site}";
		}
	' --run bash <<< '
		pandoc --filter $$(which panpipe) --filter $$(which panhandle) -f markdown+lhs -t markdown+lhs $< | unlit -f bird > $@
	'

README.md: literate/readme.md
	@nix-shell --pure -E '
		with import <nixpkgs> {};
		with haskellPackages;
		mkShell {
			name="pandoc-tools";
			buildInputs = [
			  inline-r
				data-default
				aeson
				pretty-simple
				panhandle
				pandoc-citeproc
				panpipe
				pandoc
				pkgs.which
				cabal-install
				R];
			R_LIBS_SITE = "$${builtins.readFile r-libs-site}";
		}
	' --run bash <<< '
		pandoc -t markdown_strict  --filter $$(which pandoc-citeproc) -s  $< -o $@
	'

.PHONY:clean
clean:
	rm -rf .build
	rm -rf dist*
	rm -f literate/main.hs
	rm -f src/Bandit/Tutorial.hs
	rm -f hbandit.nix
	rm -f hbandit.cabal
	rm -rf dhall-to-cabal
