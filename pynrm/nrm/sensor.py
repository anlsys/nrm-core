###############################################################################
# Copyright 2019 UChicago Argonne, LLC.
# (c.f. AUTHORS, LICENSE)
#
# This file is part of the NRM project.
# For more info, see https://xgitlab.cels.anl.gov/argo/nrm
#
# SPDX-License-Identifier: BSD-3-Clause
###############################################################################

"""Sensor Module:
    provide the core functionalities related to measuring power, energy,
    temperature and other information about the local node, using our internal
    version of the coolr code.

    This module should be the only one interfacing with coolr.
"""
from __future__ import print_function
import time
import coolr
import coolr.clr_rapl
import coolr.clr_hwmon
import coolr.clr_nodeinfo
import coolr.clr_cpufreq
import coolr.clr_misc


class SensorManager:
    """Performs sensor reading and basic data aggregation."""

    def __init__(self):
        self.nodeconfig = coolr.clr_nodeinfo.nodeconfig()
        self.nodename = self.nodeconfig.nodename
        self.cputopology = coolr.clr_nodeinfo.cputopology()
        self.coretemp = coolr.clr_hwmon.coretemp_reader()
        self.rapl = coolr.clr_rapl.rapl_reader()

    def start(self):
        self.rapl.start_energy_counter()

    def stop(self):
        self.rapl.stop_energy_counter()

    def do_update(self):
        machine_info = dict()
        machine_info['energy'] = self.rapl.sample(accflag=True)
        machine_info['temperature'] = self.coretemp.sample()
        machine_info['time'] = time.time()
        return machine_info

    def get_powerlimits(self):
        pl = self.rapl.get_powerlimits()
        # only return enabled domains
        return {k: pl[k] for k in pl if pl[k]['enabled']}

    def set_powerlimit(self, domain, value):
        self.rapl.set_powerlimit(value, domain)

    def calc_difference(self, start, end):
        diff = dict()
        for k in start.keys():
            if k not in ['time']:
                start[k.replace('p', 'package-')] = start[k]
                start.pop(k)
                end[k.replace('p', 'package-')] = end[k]
                end.pop(k)

        # Calculate energy difference
        diff['energy'] = self.rapl.diffenergy(start, end)
        # Update time elapsed
        diff['time'] = diff['energy']['time']
        # Remove 'time' field returned by function
        diff['energy'].pop('time')
        # Convert uJ to J
        diff['energy'] = {k: diff['energy'][k]/(1000000.0) for k in
                          diff['energy']}

        # Calculate power difference
        diff['power'] = self.rapl.calcpower(start, end)
        # Remove 'delta' field returned by function
        diff['power'].pop('delta')

        return diff
