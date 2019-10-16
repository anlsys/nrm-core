#!/usr/bin/env bash
p=$(nix-build -A nrm)
nix-copy-closure --to cc@129.114.108.2 $p
ssh cc@129.114.108.2 "sudo ln -s $p ~/nrm2"
