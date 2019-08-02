###############################################################################
# Copyright 2019 UChicago Argonne, LLC.
# (c.f. AUTHORS, LICENSE)
#
# This file is part of the NRM project.
# For more info, see https://xgitlab.cels.anl.gov/argo/nri
#
# SPDX-License-Identifier: BSD-3-Clause
###############################################################################

""" Msr Module: This module provides the interfaces to read and write msr
    through the msr_safe kernel module.

    Note: the msr_safe kernel module needs to
    be installed on your machine for this module to work.
"""

import os
import math
import sys
import errno
import struct
from typing import TypeVar
from nri.types.definitions import (
    FreqControl,
    _MicroWatts,
    _CoreID,
    _Return,
    Failure,
)
from nri.utils import Path

_T2 = TypeVar("_T2")

_msr_unit_location: int = 0x00000606
_msr_powerlimit_location = 0x00000610


class Msr:
    # set power limits for a specific cpu. writes are in effect
    # for all cpus on the same package.
    def setfrequencies(self, control: FreqControl) -> _Return:
        return Failure

    def set_powerlimit(self, cpu: _CoreID, watts: _MicroWatts) -> None:
        unit = math.sqrt(float(self.read(cpu, _msr_unit_location) & 0xf))
        limit = self.read(cpu, _msr_powerlimit_location)
        self.write(
            cpu,
            _msr_powerlimit_location,
            self._insertbits(limit, 0, 14, watts / unit),
        )

    def _insertbits(self, val: int, l: int, h: int, newval: float) -> int:
        w = h - l + 1
        if w < 1:
            return 0
        m = (int(1) << w) - 1
        newval = int(newval) & m
        m = (int(1) << (h + 1)) - 1 - ((int(1) << l) - 1)
        m = ~m
        return (int(val) & m) | (newval << l)

    def get_file_path(self, cpu: _CoreID) -> Path:
        return Path("/dev/cpu/%d/msr_safe" % cpu)

    # open msr file with correct privileges
    def file_open(self, filename: Path, privilege: str) -> int:
        try:
            if privilege == "r":
                fd = os.open(filename, os.O_RDONLY)
            elif privilege == "w":
                fd = os.open(filename, os.O_WRONLY)

        except OSError as e:
            if e.errno == errno.ENXIO:
                sys.exit("file_open: No such device or address " + filename)
            elif e.errno == errno.EIO:
                sys.exit("file_open: I/O error " + filename)
            elif e.errno == errno.EACCES:
                sys.exit("file_open: Permission denied " + filename)
            else:
                sys.exit("file_open: Error " + filename)

        return fd

    # read a msr
    def read(self, cpu: _CoreID, register: int) -> int:
        msrfile = self.get_file_path(cpu)
        fd = self.file_open(msrfile, "r")
        try:
            os.lseek(fd, int(register), os.SEEK_SET)
            """ read and handle binary data from msr file """
            value: int = struct.unpack("Q", os.read(fd, 8))[0]
            os.close(fd)

        except OSError as e:
            os.close(fd)
            if e.errno == errno.EIO:
                sys.exit("read: I/O error " + msrfile)
            elif e.errno == errno.EACCES:
                sys.exit("read: Permission denied " + msrfile)
            else:
                sys.exit("read: Error " + msrfile)

        return value

    # write a msr
    def write(self, cpu: _CoreID, register: int, value: _T2) -> _T2:
        msrfile = self.get_file_path(cpu)
        fd = self.file_open(msrfile, "w")
        try:
            os.lseek(fd, int(register), os.SEEK_SET)
            """ write binary data to msr file """
            os.write(fd, struct.pack("Q", value))
            os.close(fd)

        except OSError as e:
            os.close(fd)
            if e.errno == errno.EIO:
                sys.exit("write: I/O error " + msrfile)
            elif e.errno == errno.EACCES:
                sys.exit("write: Permission denied " + msrfile)
            else:
                sys.exit("write: Error " + msrfile)

        return value
