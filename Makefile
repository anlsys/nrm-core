# this file was tested using GNUMAKE >= 4.2.1.

# this is necessary for using multi-line strings as command arguments.
SHELL := $(shell which bash)

# this allows omitting newlines.
.ONESHELL:

# "nix-shell -p" constructs an expression that relies on <nixpkgs> for
# selecting attributes, so we override it.
# https://github.com/NixOS/nix/issues/726#issuecomment-161215255
NIX_PATH := nixpkgs=./.

############################# SECTION: defining recursive targets

.PHONY: hsnrm/%
hsnrm/%:
	$(MAKE) -C hsnrm $*

.PHONY: pynrm/%
pynrm/%:
	$(MAKE) -C pynrm $*


############################# all: triggers all builds.
all: hsnrm/all libnrm/all

############################# pre-commit: triggers all pre-commit rules. root and recursive
.PHONY: pre-commit
pre-commit: hsnrm/pre-commit\
	pynrm/pre-commit\
	libnrm/pre-commit\
	dhall/format\
	shellcheck\
	nixfmt\
	.gitlab-ci.yml

.PHONY: notebooks
notebooks:
	@nix-shell default.nix --pure -A hack --run <<< bash '
		notebooks/batchnb.py notebooks/configuration.ipynb
		jupyter nbconvert doc/notebooks/notebooks/configuration.ipynb --output-dir=doc/notebooks/notebooks
		rm doc/notebooks/notebooks/configuration.ipynb
		jupyter nbconvert notebooks/tutorial.ipynb --output-dir=doc/notebooks/notebooks
		jupyter nbconvert notebooks/internal-control.ipynb --output-dir=doc/notebooks/notebooks
	'

dhrun/%:
	rm -f hsnrm/.ghc*
	@nix-shell --pure -p nrm -p dhrun -p bash --run "dhrun -i" <<< '
		let all = ./dev/dhrun/all-tests.dh
			"../dev/dhrun/assets/"
			"../resources/defaults/Cfg.dhall // { verbose=<Normal|Verbose|Debug>.Debug }"
			"../resources/examples/"
		in all.exitcode
	'

############################# SECTION: source tooling

.PHONY: nixfmt
nixfmt:
	@nix-shell --pure -p fd nixfmt --run bash <<< '
		RETURN=0
		for F in $$(fd -e nix); do
			nixfmt -c $$F
			if [ $$? -ne 0 ]; then
				echo "[!] $$F does not pass nixfmt format check. Formatting.." >&2
				nixfmt $$F
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

############################# SECTION: libnrm pseudo-recursive targets (actual directory uses autotools)

.PHONY: libnrm/autotools
libnrm/autotools: libnrm/src/nrm_messaging.h
	nix-shell -E '
		let pkgs = import <nixpkgs> {});
		in pkgs.libnrm.overrideAttrs (o:{
			preBuild="";
		})
	' --run bash <<< '
		set -e
		cd libnrm
		./autogen.sh
		./configure --enable-pmpi CC=mpicc FC=mpifort CFLAGS=-fopenmp
		make
	'

libnrm/src/nrm_messaging.h: hsnrm/pre-commit
	cp hsnrm/resources/nrm_messaging.h libnrm/src/nrm_messaging.h

.PHONY:libnrm/pre-commit
libnrm/pre-commit: libnrm/clang-format libnrm/src/nrm_messaging.h

.PHONY: libnrm/clang-format
libnrm/clang-format:
	@nix-shell --pure -p fd clang-tools --run bash <<< '
		cd libnrm
		RETURN=0
		for F in $$(fd -e c); do
			clang-format < $$F | cmp -s $$F -
			if [ $$? -ne 0 ]; then
				echo "[!] $$F does not pass clang-format format check." >&2
				RETURN=1
			fi
		done
		if [ $$RETURN -ne 0 ]; then exit 1; fi
	'

############################ SECTION: gitlab-ci helpers

.gitlab-ci.yml: .gitlab-ci.dhall
	echo "#automatically generated from .gitlab-ci.dhall, changes will be erased." > .gitlab-ci.yml
	@nix-shell -p haskellPackages.dhall-json --run dhall-to-yaml <<< ' ./.gitlab-ci.dhall ' >> .gitlab-ci.yml

.PHONY: ci
ci:
	@nix-shell -p yq jq --run bash <<< '
		for jobname in $$(yq -r "keys| .[]" .gitlab-ci.yml); do
			if [ "$$jobname" != "stages" ]; then
				gitlab-runner exec shell "$$jobname"
			fi
		done
	'

ci-%:
	@nix-shell default.nix -A hack --run bash <<< '
		gitlab-runner exec shell "$*"
	'

