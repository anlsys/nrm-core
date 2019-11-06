#!/usr/bin/env bash
target=$1
storepath=$(nix-build -A nrm)
echo $storepath
nix-copy-closure --to $target $storepath
ssh $target "~/.nix-profile/bin/nix-env -i $storepath"
