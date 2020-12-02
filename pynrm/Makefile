# this file was tested using GNUMAKE >= 4.2.1.

# this is necessary for using multi-line strings as command arguments.
SHELL := $(shell which bash)

# this allows omitting newlines.
.ONESHELL:

# "nix-shell -p" constructs an expression that relies on <nixpkgs> for
# selecting attributes, so we override it.
# https://github.com/NixOS/nix/issues/726#issuecomment-161215255
NIX_PATH := nixpkgs=./.

all: black

.PHONY: pre-commit
pre-commit: black

.PHONY: black
black:
	@nix-shell --pure -p python37Packages.black --run bash <<< '
	RETURN=0
	
	black bin/* nrm/*.py extra/*.py --check
	if [ $$? -ne 0 ]; then
		echo "[!] Black format fail - Formatting.." >&2
		black bin/* nrm/*.py extra/*.py
		RETURN=1
	fi
	if [ $$RETURN -ne 0 ]; then exit 1; fi
	'
