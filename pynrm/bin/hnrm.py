#!/usr/bin/env python3

import ctypes
import struct
import msgpack
import sys
import nrm.daemon
# import struct
free = ctypes.cdll.LoadLibrary("libc.so.6").free


def _make_msgpack_fun(fun):
    fun.restype = ctypes.POINTER(ctypes.c_char)

    def f(*args):
        packed = msgpack.packb(args)
        length_64bits = struct.pack(">q", len(packed))  # big-endian
        ptr = fun(length_64bits + packed)
        data_length = struct.unpack(">q", ptr[:8])[0]
        res = msgpack.unpackb(ptr[8: 8 + data_length])
        free(ptr)
        return res

    return f


class hsnrm:
    def __init__(self, libpath) -> None:
        # print(libpath)
        self.lib = ctypes.cdll.LoadLibrary(libpath)

    def __enter__(self) -> "hsnrm":
        self.lib.hs_init(0, 0)
        return self

    def __exit__(self, type: Exception, value: Exception, traceback: str) -> None:
        self.lib.hs_exit()

    def parseArgDaemonCli(self, *kwargs) -> None:
        return _make_msgpack_fun(self.lib.parseArgDaemonCli)(*kwargs)

    def verbose(self, *kwargs) -> None:
        return _make_msgpack_fun(self.lib.verbose)(*kwargs)


if __name__ == "__main__":
    with hsnrm("hsnrm/dist-newstyle/build/x86_64-linux/ghc-8.6.5/hsnrm-1.0.0/x/nrm.so/build/nrm.so/nrm.so") as h:
        c = h.parseArgDaemonCli(sys.argv[1:])
        print(c)
        c = h.parseArgDaemonCli(sys.argv[1:])
        print(c)

    # with hsnrm("hsnrm/dist-newstyle/build/x86_64-linux/ghc-8.6.5/hsnrm-1.0.0/x/nrm.so/build/nrm.so/nrm.so") as h:
        # v = h.verbose(sys.argv[1:])
        # print(v)

        # c2 = h.parseArgDaemonCli(sys.argv[1:])
        # print(c2)
        # v = h.verbose(c)
        # print(v)
        # nrm.daemon.runner(config=h.parseArgDaemonCli(sys.argv[1:]), nrmlib=h)
