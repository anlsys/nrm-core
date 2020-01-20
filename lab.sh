#!/usr/bin/env bash
nix-shell default.nix -A jupyterLabEnvironment --run jupyter-lab
