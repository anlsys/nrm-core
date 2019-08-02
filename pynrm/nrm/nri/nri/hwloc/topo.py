###############################################################################
# Copyright 2019 UChicago Argonne, LLC.
# (c.f. AUTHORS, LICENSE)
#
# SPDX-License-Identifier: BSD-3-Clause
###############################################################################

import subprocess
from typing import List
from nri.types.definitions import _PkgID, _CoreID, _PUID
from nri.types.constructors import mkPkgID, mkCoreID, mkPUID
from dataclasses import dataclass
import xml.etree.ElementTree


class Topology:
    """
    This object queries hwloc for a XML topology at instanciation time and
    exposes various XML Xpath queries.
    """

    def __init__(self, cmd: str = "hwloc-ls") -> None:
        p = subprocess.Popen(
            [cmd, "-p", "--whole-system", "--output-format", "xml"],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
        )
        stdout, stderr = p.communicate()
        self.tree = xml.etree.ElementTree.fromstring(stdout)

    def list_core_ids(self) -> List[_CoreID]:

        """
        Lists vendor core IDs. This is a discrete, discontinous (because of core
        overprovisioning) index which hwmon sysfs uses.
        """
        return [
            mkCoreID(core.attrib["os_index"])
            for core in self.tree.findall('.//*[@type="Core"]')
        ]

    def list_processing_unit_ids(self) -> List[_PUID]:

        """
        Lists (OS) Processing Unit IDs.
        """
        return [
            mkPUID(core.attrib["os_index"])
            for core in self.tree.findall('.//*[@type="PU"]')
        ]
