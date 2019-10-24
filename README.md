HNRM monorepo

### deps/building

get Nix,

- release: `nix-build -A nrm`
- dev build: `nix-shell`, then `./shake build && ./shake client`
- local CI: `nix-env -f. -iA gitlab-runner && gitlab-runner exec shell <jobname>` . see `.gitlab-ci.yml` for job names

### architecture

- `/pynrm` : daemon runtime
- `/hsnrm` : nrm.so and code generator source
- `/libnrm` : (c, fortran, python) application code
- `/resources` : everything in this directory is generated automatically by the build chains. It covers nrm.so bindings, libnrm header file, manifests format and default values (dhall types/values, yaml example), configuration example and default values (same).
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
