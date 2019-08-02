###############################################################################
# Copyright 2019 UChicago Argonne, LLC.
# (c.f. AUTHORS, LICENSE)
#
# SPDX-License-Identifier: BSD-3-Clause
###############################################################################

import os
import sys
import time
from nri.types.definitions import (
    TemperatureSamples,
    _CoreID,
    _PkgID,
    _Temperature,
    _Time,
)
from nri.types.constructors import mkCoreID, mkPkgID, mkTime
from typing import Dict, Union, NamedTuple, Optional, Callable

# an example the content of cpustat
# id                    0
# aperf     4926926121023
# mperf     4582847073452
# perf_bias             8
# ucc     281462841145699
# urc                   0
# perf_target        8448
# perf_status        8448
# pstate               33
# turbo_disengage       0
# tsc    1117245755950154


class Cpustatvals:
    time: _Time
    id_: int
    aperf: int
    mperf: int
    pstate: int
    tsc: int
    u64max: int

    def cpustatfn(self, cpuid: _CoreID) -> str:
        return "/sys/devices/system/cpu/cpu%d/cpustat/cpustat" % cpuid

    def __init__(self, cpuid: _CoreID) -> None:
        self.u64max = (1 << 64) - 1
        self.time = mkTime(time.time())
        self.cpuid = cpuid
        self.parse()

    def parse(self) -> None:
        self.time = mkTime(time.time())
        with open(self.cpustatfn(self.cpuid)) as f:
            while True:
                li = f.readline()
                if not li:
                    break
                a = li.split()
                if a[0] == "id":
                    self.id_ = int(a[1])
                if a[0] == "aperf":
                    self.aperf = int(a[1])
                if a[0] == "mperf":
                    self.mperf = int(a[1])
                if a[0] == "pstate":
                    self.pstate = int(a[1])
                if a[0] == "tsc":
                    self.tsc = int(a[1])

    def diff_u64(self, v1: int, v2: int) -> int:  # v1 - v2
        if v1 >= v2:
            return v1 - v2
        return (self.u64max - v2) + v1

    def calc_cpufreq(self, prev: "Cpustatvals") -> float:
        tsc = float(self.diff_u64(self.tsc, prev.tsc))
        aperf = float(self.diff_u64(self.aperf, prev.aperf))
        mperf = float(self.diff_u64(self.mperf, prev.mperf))
        return 1e-9 * tsc * (aperf / mperf) / (self.time - prev.time)

    def calc_aperf(self, prev: "Cpustatvals") -> float:
        return (
            self.diff_u64(self.aperf, prev.aperf)
            * 1e-9
            / (self.time - prev.time)
        )


class cpufreq_reader:
    def __init__(self) -> None:
        pucount = os.popen("hwloc-calc machine:0 -N PU").read()
        self.cpus = range(0, int(pucount) - 1)
        self.init = False

        for cpuid in self.cpus:
            tmp = Cpustatvals(mkCoreID(cpuid))  # just for cpustatfn
            statpath = tmp.cpustatfn(mkCoreID(cpuid))
            if not os.path.exists(statpath):
                # print 'Not found', statpath
                return

        self.init = True
        self.cnt = 0
        self.samples = [
            [Cpustatvals(mkCoreID(i)) for i in self.cpus],
            [Cpustatvals(mkCoreID(i)) for i in self.cpus],
        ]

        self.sample()

    def sample(self) -> None:
        if not self.init:
            return

        idx = self.cnt % 2
        for cpuid in self.cpus:
            self.samples[idx][cpuid].parse()
        self.cnt = self.cnt + 1

    def pstate(self) -> list:
        ret = [0.0 for i in self.cpus]
        if not self.init:
            return ret
        if self.cnt == 0:
            return ret

        idx = 0  # if cnt is an odd number
        if self.cnt % 2 == 0:
            idx = 1
        for cpuid in self.cpus:
            ret[cpuid] = self.samples[idx][cpuid].pstate

        return ret

    def cpufreq(self) -> list:
        ret = [0.0 for i in self.cpus]
        if not self.init:
            return ret
        if self.cnt < 2:
            return ret

        idxprev = 0
        idxcur = 1
        if (self.cnt % 2) == 1:
            idxprev = 1
            idxcur = 0

        for cpuid in self.cpus:
            ret[cpuid] = self.samples[idxcur][cpuid].calc_cpufreq(
                self.samples[idxprev][cpuid]
            )

        return ret

    def aperf(self) -> list:
        ret = [0.0 for i in self.cpus]
        if not self.init:
            return ret
        if self.cnt < 2:
            return ret

        idxprev = 0
        idxcur = 1
        if (self.cnt % 2) == 1:
            idxprev = 1
            idxcur = 0

        for cpuid in self.cpus:
            ret[cpuid] = self.samples[idxcur][cpuid].calc_aperf(
                self.samples[idxprev][cpuid]
            )

        return ret
