###############################################################################
# Copyright 2019 UChicago Argonne, LLC.
# (c.f. AUTHORS, LICENSE)
#
# This file is part of the NRM project.
# For more info, see https://xgitlab.cels.anl.gov/argo/nrm
#
# SPDX-License-Identifier: BSD-3-Clause
###############################################################################

""" DDCMPolicy Module:
    This module contains the Dynamic Duty Cycle Modulation (DDCM) based policy
    aimed at mitigating workload imbalance in parallel applications that use
    barrier synchronizations. It reduces duty cycle of cpus not on the critical
    path of execution thereby reducing energy with little or no adverse impact
    on performance.

    This implementation specifically targets Intel architecture.

    Please check your architecture specification for supported power control
    mechanisms and other information.

    Additional information:

    1. Bhalachandra, Sridutt, Allan Porterfield, and Jan F. Prins. "Using
    dynamic duty cycle modulation to improve energy efficiency in high
    performance computing." In Parallel and Distributed Processing Symposium
    Workshop (IPDPSW), 2015 IEEE International, pp. 911-918. IEEE, 2015.

    2. Porterfield, Allan, Rob Fowler, Sridutt Bhalachandra, Barry Rountree,
    Diptorup Deb, and Rob Lewis. "Application runtime variability and power
    optimization for exascale computers." In Proceedings of the 5th
    International Workshop on Runtime and Operating Systems for Supercomputers,
    p. 3. ACM, 2015.
"""

from __future__ import division
import math
import coolr
import coolr.dutycycle


class DDCMPolicy:
    """ Contains cpu-specific DDCM based power policy """

    def __init__(self, maxlevel=16, minlevel=1):
        self.maxdclevel = maxlevel
        self.mindclevel = minlevel
        # Relaxation factor
        self.relaxation = 1

        self.ddcmpolicyset = 0
        self.ddcmpolicyreset = 0

        self.dc = coolr.dutycycle.DutyCycle()

    def print_stats(self, resetflag=False):
        ddcmstats = dict()
        ddcmstats['DDCMPolicySets'] = self.ddcmpolicyset
        ddcmstats['DDCMPolicyResets'] = self.ddcmpolicyreset
        if resetflag:
            self.ddcmpolicyset = 0
            self.ddcmpolicyreset = 0

        return ddcmstats

    def execute(self, cpu, currentdclevel, computetime, totalphasetime):
        # Compute work done by cpu during current phase
        work = computetime / totalphasetime

        # Compute effective work based on current duty cycle(dc) level
        effectivework = work * self.maxdclevel / currentdclevel

        # Compute effective slow down in current phase
        effectiveslowdown = work * self.mindclevel / currentdclevel

        # Decrease or keep constant dc level in the next phase if the effective
        # work done is equal or less than 1.0
        if effectivework <= 1.0:
            self.ddcmpolicyset += 1

            # Compute by how many levels dc needs to decrease
            dcreduction = math.floor(effectivework / 0.0625) - 15

            # Compute new dc level for next phase
            if -14 < dcreduction < 0:
                # Note that dcreduction is a negative value
                newdclevel = currentdclevel + dcreduction + self.relaxation
            elif dcreduction < -13:
                # Empirical observation shows reducing dc below 18.75% leads to
                # excessive slowdown
                newdclevel = currentdclevel - 13
            else:
                # If reduction required is 0
                newdclevel = currentdclevel

        # If there was a slowdown in the last phase, then increase the duty
        # cycle level corresponding to the slowdown
        else:
            self.ddcmpolicyreset += 1

            # Compute by how many levels dc needs to increase
            dcincrease = math.floor(effectiveslowdown / 0.0625)

            newdclevel = currentdclevel + dcincrease

        # Check if new dc level computed is within permissible range, else
        # reset
        if newdclevel < self.mindclevel or newdclevel > self.maxdclevel:
            newdclevel = self.maxdclevel
        # Set the duty cycle of cpu to the new value computed
        self.dc.set(cpu, newdclevel)

        return newdclevel
