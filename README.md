HNRM monorepo

### Installation

#### Nix

the NRM client and daemon can be installed in the user environment by running
`nix-env -f. -iA nrm` from a local clone.

libnrm-instrumented applications can also be installed using `nix-env -f.  -iA stream`.

### Development

#### Nix

from a local clone:

- release: `nix-build -A nrm`
- dev build: `nix-shell`, then `./shake build and ./shake client`
- local CI: `nix-env -f. -iA gitlab-runner && gitlab-runner exec shell <jobname>` . see `.gitlab-ci.yml` for job names

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
