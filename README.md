# NRM monorepo

### Installation

the NRM client and daemon can be installed in the user environment by running
`nix-env -f. -iA nrm` from a local clone.

libnrm-instrumented applications can also be installed using `nix-env -f.  -iA <appname>`.

### Documentation

- [User documentation](http://hnrm.readthedocs.io)
- [Notebooks](doc/notebooks/notebooks)
- Shared library [haddocks](https://hnrm.readthedocs.io/en/latest/_static/haddocks/)

### Development

from a local clone, *with submodules initialized and updated*:

```
git clone https://xgitlab.cels.anl.gov/argo/hnrm.git
cd hnrm
git submodule init
git submodule update
```

##### read internal haddock documentation: 

```
$ firefox $(nix-build -A haskellPackages.nrmlib.doc)/share/doc/nrmlib-*/html/index.html
```

##### make a release build: 

```
nix-build -A nrm
```

##### enter a development environment: 

You may enter a dev env using `nix-shell`.
Those helpers are available:

- use `$ ./shake.sh build` to build the `nrm.so` shared library (also runs the code generation step for vendored resources)
- use `$ ./shake.sh client` to build the `nrm` client
- use `$ ./shake.sh pyclient` to build the `python` shared library (for use by the python module `nrm.tooling`)
- the appropriate Nix `shellHooks` are in place for you to use `nrm` and `nrmd`.

##### run CI jobs locally: 

you need a gitlab runner at the same version as your CI infrastructure.  A
minima, try `nix-env -iA nixpkgs.gitlab-runner`. You can then run a unique job
using `./ci.sh <jobname>` or all jobs using `./ci.sh`. This runs jobs on your
latest local commit.

##### deploy to a Nix-enabled remote system:

`./deploy.sh nrm user@host`

##### run jupyter-lab:

* Unexecuted: [`notebooks/`](notebooks/)
* Executed and vendored (CI checked for being up-to-date an not throwing errors): [`doc/notebooks/notebooks`](doc/notebooks/notebooks) (also contains HTML versions)

Running the notebooks in development mode:

```
./lab.sh
```

Note: `jupyterWith` takes a while to build, so if you are an advanced Nix user,
you may use [cachix](https://cachix.org/) for this artifact before running
`./lab.sh`:

```
cachix use jupyterWith
```
