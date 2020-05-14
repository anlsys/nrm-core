# NRM

## User Documentation

- [User documentation](http://hnrm.readthedocs.io)

## Installation

All the following instructions suppose using a local clone:

```
git clone https://xgitlab.cels.anl.gov/argo/hnrm.git
```

- local nix build: `nix-build -A nrm`
- local nix profile install: `nix-env -f. -iA nrm`

## Development

- building: `make`
- running CI jobs locally: `make ci-<jobname>` or all jobs using `make ci`
  (requires gitlab-runner)
- entering a development shell : `nix-shell`. The appropriate `shellHooks` are
  in place for the `nrm` and `nrmd` aliases to be used on the CLI. 
