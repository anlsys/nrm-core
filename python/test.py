#!/usr/bin/env python

from ctypes import *
import struct
import msgpack

so_file_path = './dist/build/hnrm.so/hnrm.so'

free = cdll.LoadLibrary("libc.so.6").free
lib = cdll.LoadLibrary(so_file_path)

lib.hs_init(0, 0)

# Some shortcuts
def make_msgpack_fun(fun):
    fun.restype = POINTER(c_char)

    def f(*args):
        packed = msgpack.packb(args)
        length_64bits = struct.pack(">q", len(packed)) # big-endian
        ptr = fun(length_64bits + packed)
        data_length = struct.unpack(">q", ptr[:8])[0]
        res = msgpack.unpackb(ptr[8:8+data_length])
        free(ptr)
        return res

    return f

f = make_msgpack_fun(lib.getDefaultRAPLDirsExport)
print(f())

lib.hs_exit()
