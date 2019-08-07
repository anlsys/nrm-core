monorepo version of NRM with libnrm code generation and separated logic via shared library

### architecture

- `/pynrm` : daemon runtime
- `/hsnrm` : nrm.so, code generator
- `/libnrm` : c, fortran, python app bindings
- `/gen` : generated code for nrm.so bindings and libnrm header file. symlinked into libnrm and pynrm
- `/resources` : manifests, configuration code, "cpdl".
- `/dev` : other development related workflows.

##### concern separation

###### `nrmd.py` daemon runtime
- main thread 
- rpc
- zmq
- ioloop
- holding children processes
- message *passing*
###### `nrm.so` shared library
- message schemas, serialization, deserialization
- configuration management (dhall+/yaml)
- manifest management (dhall+/yaml)
- topology management
- handware interactions
- internal daemon state management
- daemon control flow behavior decisions
- control loops
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
