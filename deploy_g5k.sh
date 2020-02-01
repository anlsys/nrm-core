#!/usr/bin/env bash
# shellcheck disable=SC2086
# shellcheck disable=SC2048
rm -f .ghc.env*
if [ -z "$IN_NIX_SHELL" ]
then
  nix-shell \
    --run "runhaskell dev/deploy_g5k.hs $*" \
    --option extra-substituters http://129.114.24.212/serve \
    --option trusted-public-keys example-nix-cache-1:HSwzbJmGDidTrax3Lvx1vMSvto04VN2O5cjfXAG9uz0=
else
  runhaskell dev/deploy_g5k.hs $*
fi
