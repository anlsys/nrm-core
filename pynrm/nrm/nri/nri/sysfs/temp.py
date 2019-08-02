###############################################################################
# Copyright 2019 UChicago Argonne, LLC.
# (c.f. AUTHORS, LICENSE)
#
# SPDX-License-Identifier: BSD-3-Clause
###############################################################################

import re
import os
import sys
from typing import (
    Dict,
    Union,
    NamedTuple,
    Optional,
    Callable,
    Pattern,
    Match,
    TypeVar,
)
from nri.sysfs.misc import readbuf
from nri.types.definitions import (
    TemperatureSamples,
    _CoreID,
    _PkgID,
    _Temperature,
)
from nri.types.constructors import mkCoreID, mkPkgID, mkTemperature
from nri.types.internal.definitions import Path
from nri.utils import fmapOpt
from nri.hwloc.topo import Topology
from dataclasses import dataclass, field
from collections import ChainMap

a = TypeVar("a")


class PackageCoretempInfo(NamedTuple):
    dir: str
    pkgtempfn: str
    coretempfns: Dict[_CoreID, str]


PkgPaths = Dict[_PkgID, Path]
CorePaths = Dict[_CoreID, Path]


class Coretemp:
    pkgpaths: PkgPaths
    corepaths: CorePaths

    def __init__(self, hwmondir: str = "/sys/class/hwmon/") -> None:

        hwmdirs = list(
            filter(
                _has_coretemp_in_name_file,
                [Path("%s%s" % (hwmondir, d)) for d in os.listdir(hwmondir)],
            )
        )

        def getPaths(
            pat: str, builder: Callable[[str], Optional[a]]
        ) -> Dict[a, Path]:
            # pattern: dictionary comprehension with side effects.
            # internal redundant calls could be memoized (unimportant).
            # mypy can not properly infer option type unpacking
            # inside list comprehensions, so there is a type ignore.
            def readOSCoreID(d: Path, m: Match) -> Optional[a]:
                with open("%s/temp%s_label" % (d, m.group(1)), "r") as f:
                    return fmapOpt(
                        lambda x: builder(x.group(1)),
                        re.search(pat, f.readline()),
                    )

            return {  # type:ignore
                readOSCoreID(d, m): _build_target_path(d, m)
                for d in hwmdirs
                for filelist in os.listdir(d)
                for m in [re.search("temp([0-9]+)_label", filelist)]
                if m is not None
                if readOSCoreID(d, m) is not None
                if os.access(_build_target_path(d, m), os.R_OK)
            }

        self.pkgpaths = getPaths("P[a-z]* id ([0-9]+)", mkPkgID)
        self.corepaths = getPaths("Core ([0-9]+)", mkCoreID)

    def sample(self) -> TemperatureSamples:
        """
        Sample the package and core temperatures.
        """
        return TemperatureSamples(
            pkg_t_celcius=_read_all_temperatures(self.pkgpaths),
            core_t_celcius=_read_all_temperatures(self.corepaths),
        )


def _read_all_temperatures(d: Dict[a, Path]) -> Dict[a, _Temperature]:
    return {
        pkgid: mkTemperature(float(readbuf(path)) / 1000.)
        for pkgid, path in d.items()
    }


def _has_coretemp_in_name_file(folder: Path) -> bool:
    return "coretemp" == readbuf("%s/name" % folder).rstrip()


def _build_target_path(d: Path, m: Match) -> Path:
    return Path("%s/temp%s_input" % (d, m.group(1)))
