###############################################################################
# Copyright 2019 UChicago Argonne, LLC.
# (c.f. AUTHORS, LICENSE)
#
# This file is part of the NRM project.
# For more info, see https://xgitlab.cels.anl.gov/argo/nrm
#
# SPDX-License-Identifier: BSD-3-Clause
###############################################################################

""" Msr Module:
    This module provides the interfaces to read and write msr through msr_safe
    kernel module.

    Note: msr_safe kernel module needs to be installed on your machine for this
    module to work. To run with root privileges change 'msr_safe' in
    get_file_name() function to 'msr'.
"""

import os
import sys
import errno
import struct


class Msr:
    # get msr file name for the cpu
    def get_file_name(self, cpu):
        return '/dev/cpu/%d/msr_safe' % cpu

    # open msr file with correct privileges
    def file_open(self, filename, privilege):
        try:
            if privilege == 'r':
                fd = os.open(filename, os.O_RDONLY)
            elif privilege == 'w':
                fd = os.open(filename, os.O_WRONLY)

        except OSError as e:
            if e.errno == errno.ENXIO:
                sys.exit('file_open: No such device or address ' + filename)
            elif e.errno == errno.EIO:
                sys.exit('file_open: I/O error ' + filename)
            elif e.errno == errno.EACCES:
                sys.exit('file_open: Permission denied ' + filename)
            else:
                sys.exit('file_open: Error ' + filename)

        return fd

    # read a msr
    def read(self, cpu, register):
        msrfile = self.get_file_name(cpu)
        fd = self.file_open(msrfile, 'r')
        try:
            os.lseek(fd, int(register), os.SEEK_SET)
            """ read and handle binary data from msr file """
            value = struct.unpack('Q', os.read(fd, 8))[0]
            os.close(fd)

        except OSError as e:
            os.close(fd)
            if e.errno == errno.EIO:
                sys.exit('read: I/O error ' + msrfile)
            elif e.errno == errno.EACCES:
                sys.exit('read: Permission denied ' + msrfile)
            else:
                sys.exit('read: Error ' + msrfile)

        return value

    # write a msr
    def write(self, cpu, register, value):
        msrfile = self.get_file_name(cpu)
        fd = self.file_open(msrfile, 'w')
        try:
            os.lseek(fd, int(register), os.SEEK_SET)
            """ write binary data to msr file """
            os.write(fd, struct.pack('Q', value))
            os.close(fd)

        except OSError as e:
            os.close(fd)
            if e.errno == errno.EIO:
                sys.exit('write: I/O error ' + msrfile)
            elif e.errno == errno.EACCES:
                sys.exit('write: Permission denied ' + msrfile)
            else:
                sys.exit('write: Error ' + msrfile)

        return value
