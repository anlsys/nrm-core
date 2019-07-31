#!/usr/bin/env python3

import ctypes
import struct
import msgpack
import sys

# so_file_path = './dist/build/hnrm.so/hnrm.so'
so_file_path = sys.argv[1]

free = ctypes.cdll.LoadLibrary("libc.so.6").free
lib = ctypes.cdll.LoadLibrary(so_file_path)

lib.hs_init(0, 0)


# Some shortcuts
def _make_msgpack_fun(fun):
    fun.restype = ctypes.POINTER(ctypes.c_char)

    def f(*args):
        packed = msgpack.packb(args)
        length_64bits = struct.pack(">q", len(packed))  # big-endian
        ptr = fun(length_64bits + packed)
        data_length = struct.unpack(">q", ptr[:8])[0]
        res = msgpack.unpackb(ptr[8 : 8 + data_length])
        free(ptr)
        return res

    return f


getDefaultRAPLDirsExport = _make_msgpack_fun(lib.getDefaultRAPLDirsExport)
print(getDefaultRAPLDirsExport())

lib.hs_exit()
