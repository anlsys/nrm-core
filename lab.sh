#!/usr/bin/env bash
echo 'advanced Nix users may use "cachix use jupyterWith" in order to speed-up this build.'
nix-shell default.nix -A jupyterLabEnvironment --run jupyter-lab
