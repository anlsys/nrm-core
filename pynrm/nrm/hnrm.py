#!/usr/bin/env python3

import ctypes
import struct
import msgpack
import sys

free = ctypes.cdll.LoadLibrary("libc.so.6").free


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


class hnrm:
    def __init__(self, libpath) -> None:
        print(libpath)
        self.lib = ctypes.cdll.LoadLibrary(libpath)

    def __enter__(self) -> "hnrm":
        self.lib.hs_init(0, 0)
        return self

    def __exit__(self, type: Exception, value: Exception, traceback: str) -> None:
        self.lib.hs_exit()

    def getDefaultRAPLDirs(self) -> None:
        return _make_msgpack_fun(self.lib.getDefaultRAPLDirs)()


if __name__ == "__main__":
    with hnrm(sys.argv[1]) as h:
        print(h.getDefaultRAPLDirs())
