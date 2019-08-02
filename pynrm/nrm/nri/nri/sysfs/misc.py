###############################################################################
# Copyright 2019 UChicago Argonne, LLC.
# (c.f. AUTHORS, LICENSE)
#
# SPDX-License-Identifier: BSD-3-Clause
###############################################################################

import time


def readbuf(fn: str) -> str:
    with open(fn) as f:
        return f.readline()
