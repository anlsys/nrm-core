#!/usr/bin/env bash
host=$1

drv=$(nix-instantiate --quiet dev/default.nix --attr jupyterLabEnvironment)
nix-copy-closure --include-outputs --to $host $drv

drv=$(nix-instantiate --quiet dev/default.nix --attr hack)
nix-copy-closure --include-outputs --to $host $drv

drv=$(nix-instantiate --quiet dev/default.nix --attr nrm)
nix-store --realize $drv --quiet
nix-copy-closure --include-outputs --to $host $drv
