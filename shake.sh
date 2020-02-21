#!/usr/bin/env bash
# shellcheck disable=SC2086
# shellcheck disable=SC2048
rm -f .ghc.env*
if [ -z "$IN_NIX_SHELL" ]
then
  nix-shell -A hack --run "runhaskell dev/shake.hs $*"
else
  runhaskell dev/shake.hs $*
fi
