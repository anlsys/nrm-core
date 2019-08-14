monorepo version of NRM with libnrm code generation and separated logic via shared library

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

### dev

#### structure

- hsnrm dependencies

#### workflow

Obtain dependencies:
```
$ nix-shell
```

build hnrm.so and perform code generation step
```
[nix-shell:nrm/]$ ./shake build
```
