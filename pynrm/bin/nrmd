#!/usr/bin/env python3

import sys
from nrm.nrmlib import NrmLib
import nrm.daemon

with NrmLib("hsnrm/dist-newstyle/build/x86_64-linux/ghc-8.6.5/hsnrm-1.0.0/x/nrm.so/build/nrm.so/nrm.so") as lib:
    nrm.daemon.runner(lib.cli(sys.argv[1:]), lib)
