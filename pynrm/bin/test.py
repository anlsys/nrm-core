#!/usr/bin/env python3


from haskell import Haskell

import sys

with Haskell("hsnrm/dist-newstyle/build/x86_64-linux/ghc-8.6.5/hsnrm-1.0.0/x/nrm.so/build/nrm.so/nrm.so") as lib:
    al = sys.argv[1:]
    c = lib.parseArgDaemonCli(al)
    print(c)
    print(lib.verbose(c))
    # print(lib.someFunc(2))
    # ranli = [2, 1, 4, 5, 1]
    # print("pre sorting {}".format(ranli))
# print(lib.sortIt(ranli))
