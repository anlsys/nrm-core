# NRM monorepo

## User Documentation

- [User documentation](http://hnrm.readthedocs.io)

## Installation/Quickstart

All the following instructions suppose using a local clone *with submodules*:

```
git clone --recurse-submodules https://xgitlab.cels.anl.gov/argo/hnrm.git
```

- local nix build: `nix-build -A nrm`
- local nix profile install: `nix-env -f. -iA nrm`

## Development/Local builds

- running CI jobs locally: `make ci-<jobname>` or all jobs using `make ci`
  (requires gitlab-runner)
- entering a development shell : `nix-shell`
- use `make build` to build the `nrm.so` shared library
- use `make codegen` to regenerate vendored `./resources`
- use `make client` to build the `nrm` client
- use `make pyclient` to build the Python shared library (for use by the
  python module `nrm.tooling`)
- opening jupyter notebooks: `nix-shell default.nix -A expe`, which provisions
  `jupyter-notebook`.

the appropriate Nix `shellHooks` are in place in the `hack` nix derivation for
you to use the development build using the `nrm` and `nrmd` aliases on the CLI. 
