#!/usr/bin/env python

from ctypes import *
import struct
import msgpack

def wrap_into_msgpack(foreign_fun):
    foreign_fun.restype = c_char_p

    def wrapped_fun(*args):
        packed = msgpack.packb(args)
        length_64bits = struct.pack("q", len(packed)) # native-endian
        ptr = fun(length_64bits + packed)
        data_length = cast(ptr[:8], POINTER(c_longlong))[0]
        res = msgpack.unpackb(ptr[8:8+data_length])
        free(ptr)
        return res

    return wrapped_fun

getDefaultRAPLDirs = wrap_into_msgpack(cdll.LoadLibrary('./dist/build/hnrm.so/hnrm.so').getDefaultRAPLDirs_export)

print(getDefaultRAPLDirs())
