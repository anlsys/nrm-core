node resource manager experimental monorepo

### architecture

- `/libnrm` : c, fortran, python app bindings (hosts generated code in `libnrm/src/messaging.h`)
- `/pynrm` : runtime (hosts generated code in `pynrm/nrm/pyhnrm.py`)
- `/hsnrm` : nrm.so and code generator
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


### build workflow

```
$ nix-shell
[nix-shell:nrm/]$ ./shake build nrm.so
```

### dev workflow

- hsnrm dependencies
