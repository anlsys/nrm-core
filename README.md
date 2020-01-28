# NRM monorepo

## Installation

the NRM client and daemon can be installed in the (Nix) user environment by
running `./install.sh` from a local clone. This runs a `nix-env -i` command
with additional options to use our binary cache. 

## User Documentation

- [User documentation](http://hnrm.readthedocs.io)
- Shared library [haddocks](https://hnrm.readthedocs.io/en/latest/_static/haddocks/)

## Development

All the following instructions suppose using a local clone, *with submodules
initialized and updated*:

```
git clone https://xgitlab.cels.anl.gov/argo/hnrm.git
cd hnrm
git submodule init
git submodule update
```

- opening jupyter notebooks: `$ ./lab.sh`, then navigate to the `notebooks` folder
- entering a development shell : `$ ./shell.sh`
- local release nix build: `$ ./build.sh`
- local nix profile install: `$ ./install.sh`
- deploy to a remote nix-enabled system: `./deploy.sh nrm user@host`
- running CI jobs: `./ci.sh <jobname>` or all jobs using `./ci.sh`

Local development builds can be obtained using the following:

- use `$ ./shake.sh build` to build the `nrm.so` shared library (also runs the code generation step for vendored resources)
- use `$ ./shake.sh client` to build the `nrm` client
- use `$ ./shake.sh pyclient` to build the Python shared library (for use by the python module `nrm.tooling`)
- the appropriate Nix `shellHooks` are in place for you to use `nrm` and `nrmd` after running `./shell.sh`.
