import struct
from contextlib import contextmanager
from ctypes import CDLL, POINTER, LibraryLoader, c_char

import msgpack


class HSO(CDLL):

    exportPattern = "{name}Export"

    def __init__(self, *args, **kwargs):
        return super(HSO, self).__init__(*args, **kwargs)

    def wrap_into_msgpack(self, fun):
        fun.restype = POINTER(c_char)

        def wrapped_fun(*args):
            packed = msgpack.packb(args)
            length_64bits = struct.pack(">q", len(packed))
            ptr = fun(length_64bits + packed)
            data_length = struct.unpack(">q", ptr[:8])[0]
            res = msgpack.unpackb(ptr[8:8 + data_length], raw=False)
            self.free(ptr)
            return res

        return wrapped_fun

    def __getattr__(self, name):
        try:
            return super(HSO, self).__getattr__(name)
        except AttributeError as e:
            expName = self.exportPattern.format(name=name)
            try:
                fun = super(HSO, self).__getattr__(expName)
                return self.wrap_into_msgpack(fun)
            except AttributeError:
                print("NRM's python code tried to access an undefined symbol in nrm.so:")
                raise e


@contextmanager
def NrmLib(library, exportPattern="{name}Export"):
    hdll = LibraryLoader(HSO)
    hdll.exportPattern = exportPattern

    lib = hdll.LoadLibrary(library)
    lib.hs_init(0, 0)
    yield lib
    lib.hs_exit()
