node resource manager experimental monorepo

### architecture

- `/pynrm` : daemon runtime
- `/hsnrm` : nrm.so, code generator
- `/libnrm` : c, fortran, python app bindings
- `/gen` : generated code for nrm.so bindings and libnrm header file. symlinked into libnrm and pynrm
- `/resources` : manifests, configuration code, "cpdl".
- `/dev` : other development related workflows.

at runtime
`nrm.so` (hs) <-dlopen msgpack-> `nrmpy` bindings <-py-> nrmd (py) <-downstream zmq ipc json-> libnrm.so (c)
                                                                   <--upstream zmq tcp json--> nrm (hs)
              <--------------------------------------------------> <--upstream zmq tcp json--> pycpld (py)

at dev time
`/hsnrm` -generates-> libnrm.so
         -generates-> nrm.so
         -generates-> nrmpy bindings

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
