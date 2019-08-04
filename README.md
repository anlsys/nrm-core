node resource manager experimental architecture

at runtime
nrm.so (hs) <-dlopen msgpack-> nrmd (py) <-downstream zmq ipc json-> libnrm.so (c)
                                         <--upstream zmq tcp json--> nrm (hs)

at dev time
nrm (hs) -generates-> libnrm.so
         -generates-> nrm.so
         -generates-> nrmpy bindings ->
