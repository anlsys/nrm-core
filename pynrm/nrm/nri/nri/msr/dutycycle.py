###############################################################################
# Copyright 2019 UChicago Argonne, LLC.
# (c.f. AUTHORS, LICENSE)
#
# This file is part of the NRM project.
# For more info, see https://xgitlab.cels.anl.gov/argo/nri
#
# SPDX-License-Identifier: BSD-3-Clause
###############################################################################

""" DutyCycle Module:
    This module contains functions to set duty cyle of each cpu in intervals
    of 6.25% min = 6.25% max(default) = 100%

    Value   -   Duty Cycle
    0       -       100.0%
    1       -        6.25%
    2       -        12.5%
    3       -       18.75%
    .
    .
    .
    15      -       93.75%
    16      -       100.0%

    There are also functions to reset the duty cycle of a cpu and check the
    current value of duty cycle
"""

import nri.msr.msr
from nri.types.definitions import _CoreID


class DutyCycle:
    def __init__(self) -> None:
        self.register = 0x19A
        self.msr = nri.msr.msr.Msr()

    # set duty cycle of a cpu
    def set(self, cpu: _CoreID, value: int) -> None:
        if 0 < value < 16:
            self.msr.write(cpu, self.register, 16 + value)
        else:
            self.msr.write(cpu, self.register, 0)

    # reset duty cycle of a cpu
    def reset(self, cpu: _CoreID) -> None:
        self.msr.write(cpu, self.register, 0)

    # check current duty cycle value
    def check(self, cpu: _CoreID) -> int:
        return self.msr.read(cpu, self.register)
