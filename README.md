# hbandit

Warning: Unmaintained (but [Nix](https://nixos.org)-pinned).

Multi-Armed Bandit implementations: UCB & EXP family of algorithms, BwCR.

## Install

Provision using your Cabal or Nix toolchain. Both cabal file and nix packages
are provided in the repository.

Dependencies: hackage packages, glpk

## Tests

Provision using Nix (`nix-shell`) and run notebook
[validation/benchmark.ipynb](validation/benchmark.ipynb).
 
Dependencies: Install dependencies + SMPyBandits
