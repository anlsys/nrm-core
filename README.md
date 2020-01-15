# NRM monorepo

### Installation

the NRM client and daemon can be installed in the user environment by running
`nix-env -f. -iA nrm` from a local clone.

libnrm-instrumented applications can also be installed using `nix-env -f.  -iA stream`.

### Documentation

- [User documentation](http://hnrm.readthedocs.io)
- Shared library [haddocks](https://hnrm.readthedocs.io/en/latest/_static/haddocks/)

### Development

from a local clone, *with submodules initialized and updated*:

##### read internal haddock documentation: 

```
$ firefox $(nix-build -A haskellPackages.nrmlib.doc)/share/doc/nrmlib-*/html/index.html
```

##### make a release build: 

```
nix-build -A nrm
```

##### enter a development environment: 

enter a dev env using:

```
nix-shell
```

then:

- use `$ ./shake.sh build` to build the `nrm.so` shared library
- use `$ ./shake.sh client` to build the `nrm` client
- the appropriate Nix `shellHooks` are in place for you to use `nrm` and `nrmd`.

##### run CI jobs locally: 

you need a gitlab runner at the same version as your CI infrastructure.  A
minima, try `nix-env -iA nixpkgs.gitlab-runner`. You can then run a unique job
using `./ci <jobname>` or all jobs using `./ci`. This runs jobs on
your latest local commit.


##### install NRM to a (Nix-enabled) remote system user environment:

`./deploy nrm user@host`

##### jupyter notebooks:

Tutorial Notebooks:
* Unexecuted: [`notebooks/raw`](notebooks/raw)
* Executed: [`notebooks/executed`](notebooks/executed)

Exploratory Notebooks:
* Unexecuted: [`notebooks/exploratory`](notebooks/exploratory)

* Running the notebooks in development mode:

```
./shake.sh build
./shake.sh client
./shake.sh pyclient
nix-shell --run "jupyter-notebook <notebook file>"
```
