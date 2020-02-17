#!/usr/bin/env bash
attr=$1
target=$2
storepath=$(nix-build -A "$attr")
echo "$storepath"
nix-copy-closure --to "$target" "$storepath"
ssh "$target" '$HOME/.nix-profile/bin/nix-env -i "$storepath"'