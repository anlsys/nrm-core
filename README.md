node resource manager experimental monorepo


at runtime
nrm.so (hs) <-dlopen msgpack-> nrmpy bindings <-py-> nrmd (py) <-downstream zmq ipc json-> libnrm.so (c)
                                                               <--upstream zmq tcp json--> nrm (hs)
            <-----------------------------------  -----------> <--upstream zmq tcp json--> pycpld (py)

at dev time
nrm (hs) -generates-> libnrm.so
         -generates-> nrm.so
         -generates-> nrmpy bindings
