#!/usr/bin/env python
#
# misc. classes, functions
#
# Contact: Kazutomo Yoshii <ky@anl.gov>
#

###############################################################################
# Copyright 2019 UChicago Argonne, LLC.
# (c.f. AUTHORS, LICENSE)
#
# This file is part of the NRM project.
# For more info, see https://xgitlab.cels.anl.gov/argo/nrm
#
# SPDX-License-Identifier: BSD-3-Clause
###############################################################################


import os
import sys
import re
import time


def readbuf(fn):
    for retry in range(0, 10):
        try:
            f = open(fn)
            l = f.readline()
            f.close()
            return l
        except:
            time.sleep(0.01)
            continue
    return ''


def readuptime():
    f = open('/proc/uptime')
    l = f.readline()
    v = l.split()
    return float(v[0])
