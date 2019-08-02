###############################################################################
# Copyright 2019 UChicago Argonne, LLC.
# (c.f. AUTHORS, LICENSE)
#
# SPDX-License-Identifier: BSD-3-Clause
###############################################################################

import os
import sys
import re
import time
import getopt

from typing import Dict, Union, Pattern, NamedTuple
from nri.types.constructors import mkMicroJoules, mkMicroWatts
from nri.types.definitions import (
    RAPLConfig,
    RAPLPackageConfig,
    _PkgID,
    EnergySamples,
    _MicroJoules,
    _MicroWatts,
    PcapControl,
    _Return,
    Success,
    Failure,
)
from nri.utils import Path, readStr, readInt, readBool, writeInt


class EnergyRead(NamedTuple):
    time: float
    reads: EnergySamples


class Rapl:
    dirs: Dict[_PkgID, Path]
    lastpower: dict
    max_energy_range_uj_d: Dict[_PkgID, _MicroJoules]
    prev_e: EnergyRead
    rapldir: Path = Path("/sys/devices/virtual/powercap/intel-rapl")
    re_domain: Pattern[str] = re.compile("package-([0-9]+)(/\S+)?")
    totalenergy: dict
    initialized: bool

    def __init__(self) -> None:
        self.dirs = {}
        self.max_energy_range_uj_d = {}

        self.initialized = True
        if not os.path.exists(self.rapldir):
            self.initialized = False
            return

        for d1 in os.listdir(self.rapldir):
            dn = "%s/%s" % (self.rapldir, d1)
            fn = dn + "/name"
            if os.access(fn, os.R_OK):
                li1 = readStr(Path(fn)).strip()
                if re.search("package-[0-9]+", li1):
                    self.dirs[_PkgID(int(li1[len("package-") :]))] = Path(dn)

        for k in sorted(self.dirs.keys()):
            fn = self.dirs[k] + "/max_energy_range_uj"
            with open(fn) as f:
                self.max_energy_range_uj_d[(k)] = mkMicroJoules(
                    int(f.readline())
                )
        self.prev_e = self.readenergy()
        return

    def readenergy(self) -> EnergyRead:
        return EnergyRead(
            time=time.time(),
            reads=EnergySamples(
                {
                    pkgid: mkMicroJoules(
                        int(readStr(Path(dirname + "/energy_uj")))
                    )
                    for (pkgid, dirname) in self.dirs.items()
                }
            ),
        )

    def read_config(self) -> RAPLConfig:
        return RAPLConfig(
            {
                pkgid: RAPLPackageConfig(
                    enabled=readBool(Path(dirpath + "/enabled")),
                    constraint_0_max_power_uw=mkMicroWatts(
                        readInt(Path(dirpath + "/constraint_0_max_power_uw"))
                    ),
                    constraint_0_name=readStr(
                        Path(dirpath + "/constraint_0_name")
                    ),
                    constraint_0_time_window_us=readInt(
                        Path(dirpath + "/constraint_0_time_window_us")
                    ),
                    constraint_1_max_power_uw=mkMicroWatts(
                        readInt(Path(dirpath + "/constraint_1_max_power_uw"))
                    ),
                    constraint_1_name=readStr(
                        Path(dirpath + "/constraint_0_name")
                    ),
                    constraint_1_time_window_us=readInt(
                        Path(dirpath + "/constraint_1_time_window_us")
                    ),
                )
                for pkgid, dirpath in self.dirs.items()
            }
        )

    def diffenergy(self, e1: EnergyRead, e2: EnergyRead) -> EnergyRead:
        return EnergyRead(
            time=e2.time - e1.time,
            reads=EnergySamples(
                {
                    pkgid: (
                        mkMicroJoules(e2.reads[pkgid] - e1.reads[pkgid])
                        if e2.reads[pkgid] > e1.reads[pkgid]
                        else mkMicroJoules(
                            self.max_energy_range_uj_d[pkgid]
                            - e1.reads[pkgid]
                            + e2.reads[pkgid]
                        )
                    )
                    for pkgid in self.max_energy_range_uj_d
                }
            ),
        )

    def sample(self) -> EnergySamples:
        e = self.readenergy()
        de = self.diffenergy(self.prev_e, e)
        return de.reads

    def setpcaps(self, pcaps: PcapControl) -> _Return:
        if self.initialized:
            for pkgid, pcapValue in pcaps.items():
                writeInt(
                    Path(
                        self.dirs[pkgid]
                        + ("/constraint_%d_power_limit_uw" % pkgid)
                    ),
                    pcapValue,
                )
            return Success
        else:
            return Failure
