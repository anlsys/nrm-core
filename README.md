# NRM monorepo

### Installation

the NRM client and daemon can be installed in the user environment by running
`nix-env -f. -iA nrm` from a local clone.

libnrm-instrumented applications can also be installed using `nix-env -f.  -iA stream`.

### Development

from a local clone:

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

- use `$ ./shake build` to build the `nrm.so` shared library
- use `$ ./shake client` to build the `nrm` client
- the appropriate Nix `shellHooks` are in place for you to use `nrm` and `nrmd`.

##### run CI jobs locally: 

you need a gitlab runner at the same version as your CI infrastructure.  A
minima, try `nix-env -iA nixpkgs.gitlab-runner`. You can then run a unique job
using `./ci <jobname>` or all jobs using `./ci`. This runs jobs on
your latest local commit.

### architecture

- `/pynrm` : daemon runtime
- `/hsnrm` : nrm.so, client and code generator haskell sources
- `/libnrm` : (c, fortran) application code
- `/resources` : everything in this directory is generated automatically by the build chains. Has libnrm header file, manifests format and default values (dhall types/values, yaml example), configuration example and default values (same).
- `/dev` : other development related workflows.

##### concern separation

###### `nrmd.py` daemon runtime
- main thread 
- zmq
- starting/holding children processes
- ioloop, callbacks
###### `nrm.so` shared library
- message schemas, serialization, deserialization
- configuration management (dhall/yaml)
- manifest management (dhall/yaml)
- topology management
- handware interactions
- internal daemon state management
- daemon control flow behavior decisions
- control strategies
###### `hsnrm`
- code generation for libnrm (c headers, python downstream schema)
###### `libnrm`
- sending progress reports
