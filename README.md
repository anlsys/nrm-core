# NRM monorepo

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

- local nix profile install: `./install.sh` (`nix-env -f. -iA nrm` if binary cache is down)
- entering a development shell : `./shell.sh` (`nix-shell` if binary cache is down)
- opening jupyter notebooks: `./shell.sh`, then run `jupyter-notebook` and navigate to the `notebooks` folder
- local release nix build: `./build.sh` (`nix-build -A nrm` if binary cache is down)
- deploy to a remote nix-enabled system: `./deploy_nix.sh nrm user@host`
- deploy a g5k notebook setup: `./deploy_g5k.sh nrm user@host`
- running CI jobs: `./ci.sh <jobname>` or all jobs using `./ci.sh`

Local development builds can be obtained using the following:

- use `./shake.sh build` to build the `nrm.so` shared library (also runs the code generation step for vendored resources)
- use `./shake.sh client` to build the `nrm` client
- use `./shake.sh pyclient` to build the Python shared library (for use by the python module `nrm.tooling`)
- the appropriate Nix `shellHooks` are in place for you to use `nrm` and `nrmd` after running `./shell.sh`.

## CI

- [gitlab-ci(master branch)](https://xgitlab.cels.anl.gov/argo/hnrm/pipelines/master/latest)
- [gitlab-ci(dev branch)](https://xgitlab.cels.anl.gov/argo/hnrm/pipelines/dev/latest)
- [hydra(master branch)](http://129.114.24.212/jobset/nrm/master#tabs-jobs)
- [hydra(dev branch)](http://129.114.24.212/jobset/nrm/dev#tabs-jobs)
