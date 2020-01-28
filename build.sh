#!/usr/bin/env bash
nix-build -A nrm --option extra-substituters http://129.114.24.212 --option trusted-public-keys example-nix-cache-1:HSwzbJmGDidTrax3Lvx1vMSvto04VN2O5cjfXAG9uz0=
