# hbandit

Multi-Armed Bandit implementations: UCB & EXP family of algorithms, BwCR.

## Install

Provision using either Cabal or Nix toolchain. Both cabal file and nix packages
are provided in the repository.

Dependencies: hackage packages, glpk

## Notebooks

Provision using Nix (`nix-shell`) and run notebook
[validation/benchmark.ipynb](validation/benchmark.ipynb). Nix one-liner:

```
nix-shell "https://xgitlab.cels.anl.gov/argo/hbandit/-/archive/master/hbandit-master.tar.gz" --pure -A validation --arg useGhcide false --run 'jupyter-notebook $NOTEBOOKS/benchmark.ipynb'
```
 

## Doc

[haddocks](https://hbandit.readthedocs.io/en/latest/_static/haddocks/)
