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
- configuration management
- manifest management
- topology management
- handware interactions
- internal daemon state management
- daemon control flow behavior decisions
- control loops
###### `hsnrm`
- code generation for libnrm headers, nrmpy bindings
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

#### hsnrm structure

- `Nrm.Types.Topology`
- `Nrm.Types.Units`
- `Nrm.Types.Container`
- `Nrm.Types.Application`
- `Nrm.Types.NrmState`
- `Nrm.Types.Configuration.Internal`
- `Nrm.Types.Configuration.Yaml`
- `Nrm.Types.Configuration.Dhall`
- `Nrm.Types.Manifest.Internal`
- `Nrm.Types.Manifest.Yaml`
- `Nrm.Types.Manifest.Dhall`
- `Nrm.Types.Messaging.DownstreamEvent`
- `Nrm.Types.Messaging.UpstreamPub`
- `Nrm.Types.Messaging.UpstreamReq`
- `Nrm.Types.Messaging.UpstreamRep`
- `Nrm.Containers`
- `Nrm.Containers.Class`
- `Nrm.Containers.Nodeos`
- `Nrm.Containers.Singularity`
- `Nrm.Containers.Dummy`
- `Nrm.Node.Hwloc`
- `Nrm.Node.Sysfs`
- `Nrm.Node.Internal.Sysfs`
- `Nrm.Control`
- `Nrm.Argparse.Daemon`
- `Nrm.Argparse.Client`
- `Nrm.Behavior`
- `Nrm.Version`
