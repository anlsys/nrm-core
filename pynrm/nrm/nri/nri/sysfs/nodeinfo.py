###############################################################################
# Copyright 2019 UChicago Argonne, LLC.
# (c.f. AUTHORS, LICENSE)
#
# SPDX-License-Identifier: BSD-3-Clause
###############################################################################

import os
import sys
import re
import socket

# local
from nri.sysfs.misc import readbuf
from typing import List, Union

#
# Once instantiated, the following values are avaialble
#
# onlinecpus : a list holds all online cpus
# pkgcpus    : a dict holds per pkg cpus.  the key of the dict are pkgids
# nodecpus   : a dict holds per node cpus. the key of the dict are nodeids
#
# limitation: no support for runtime change
#


class Nodeconfig:
    def __init__(self) -> None:
        self.hostname = socket.gethostname()
        self.nodename = self.hostname.split(".")[0]
