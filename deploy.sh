#!/usr/bin/env bash
target=$1
storepath=$(nix-build -A nrm)
nix-copy-closure --to $target $storepath
ssh $target "sudo rm -rf ~/nrm && ln -s $storepath ~/nrm"
