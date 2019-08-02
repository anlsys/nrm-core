###############################################################################
# Copyright 2019 UChicago Argonne, LLC.
# (c.f. AUTHORS, LICENSE)
#
# SPDX-License-Identifier: BSD-3-Clause
###############################################################################

from nri.sysfs.nodeinfo import Nodeconfig
from nri.sysfs.rapl import Rapl
from nri.sysfs.temp import Coretemp
from nri.msr.msr import Msr
from nri.hwloc.topo import Topology

from typing import Dict, NamedTuple, Optional
from dataclasses import dataclass, field

from nri.types.definitions import (
    PcapControl,
    FreqControl,
    MachineInfo,
    RAPLConfig,
    _Time,
    _Return,
    Failure,
)

from nri.utils import time, tryOpt, unpackOpt, fmapOpt_, fmapOpt


# the main interface class. python3.7 dataclasses:
# https://docs.python.org/3/library/dataclasses.html
@dataclass
class node:
    nodename: str = field(init=False)
    _time_last: _Time = field(init=False)
    topology: Topology = field(init=False, default_factory=Topology)
    _nodeconfig: Nodeconfig = field(init=False, default_factory=Nodeconfig)
    _msr: Optional[Msr] = field(init=False, default=tryOpt(Msr))
    _rapl: Optional[Rapl] = field(init=False, default=tryOpt(Rapl))
    _coretemp: Coretemp = field(init=False, default_factory=Coretemp)

    def __enter__(self) -> "node":
        self.nodename = self._nodeconfig.nodename
        self._time_last = time()
        return self

    def __exit__(
        self, type: Exception, value: Exception, traceback: str
    ) -> None:
        pass

    def read_rapl_config(self) -> Optional[RAPLConfig]:
        return fmapOpt(lambda x: x.read_config(), self._rapl)

    def do_control_pcap(self, control: PcapControl) -> _Return:
        return unpackOpt(
            fmapOpt(lambda x: x.setpcaps(control), self._rapl), Failure
        )

    def do_control_freq(self, control: FreqControl) -> _Return:
        return unpackOpt(
            fmapOpt(lambda x: x.setfrequencies(control), self._msr), Failure
        )

    def sample(self) -> MachineInfo:
        r = MachineInfo(
            energySamples=fmapOpt(lambda x: x.sample(), self._rapl),
            tempSamples=self._coretemp.sample(),
            time=time(),
            time_last=self._time_last,
        )
        self._time_last = r.time
        return r
