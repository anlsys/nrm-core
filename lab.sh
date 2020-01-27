#!/usr/bin/env bash
echo 'you may want to run "cachix use jupyterWith" to speed-up this build.'
nix-shell default.nix -A jupyterLabEnvironment --run jupyter-lab
