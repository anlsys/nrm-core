#!/usr/bin/env python
#
# coolr hwmon related codes
#
# This code requires the coretemp driver for temperature reading
#
# Contact: Kazutomo Yoshii <ky@anl.gov>
#

###############################################################################
# Copyright 2019 UChicago Argonne, LLC.
# (c.f. AUTHORS, LICENSE)
#
# This file is part of the NRM project.
# For more info, see https://xgitlab.cels.anl.gov/argo/nrm
#
# SPDX-License-Identifier: BSD-3-Clause
###############################################################################


import re
import os
import sys
import numpy as np # type: ignore
from coolr.clr_nodeinfo import *


class coretemp_reader:
    def parse_pkgtemp(self, fn):
        retval = -1
        try:
            f = open(fn, "r")
        except:
            return retval
        l = f.readline()
        m = re.search('Physical id ([0-9]+)', l)
        if m:
            retval = int(m.group(1))
        f.close()
        return retval

    def parse_coretemp(self, fn):
        retval = -1
        try:
            f = open(fn, "r")
        except:
            return retval
        l = f.readline()
        m = re.search('Core ([0-9]+)', l)
        if m:
            retval = int(m.group(1))
        f.close()
        return retval

    hwmondir = '/sys/class/hwmon/'

    class coretempinfo:
        def __init__(self):
            self.dir = ''
            self.coretempfns = {}  # use coreid as key
            self.pkgtempfn = ''

    def __init__(self):
        self.outputpercore(True)

        self.coretemp = {}  # use pkgid as  key
        for d1 in os.listdir(self.hwmondir):
            # try to check see if 'name' contains 'coretemp'
            tmpdir = "%s%s" % (self.hwmondir, d1)
            drivername = readbuf("%s/name" % tmpdir).rstrip()
            if not drivername == "coretemp":
                continue

            pkgid = -1
            coretempfns = {}
            pkgtempfn = ''
            # parse all temp*_label files
            for d2 in os.listdir(tmpdir):
                m = re.search('temp([0-9]+)_label', d2)
                if m:
                    tempid = int(m.group(1))
                    coreid = self.parse_coretemp("%s/%s" % (tmpdir, d2))
                    if coreid >= 0:
                        coretempfns[coreid] = "%s/temp%d_input" % (
                            tmpdir, tempid)
                    else:  # possibly pkgid
                        pkgtempfn = "%s/temp%d_input" % (tmpdir, tempid)
                        pkgid = self.parse_pkgtemp("%s/%s" % (tmpdir, d2))
                        if pkgid < 0:
                            print('unlikely: ', pkgtempfn)

            cti = self.coretempinfo()
            cti.dir = tmpdir
            cti.coretempfns = coretempfns
            cti.pkgtempfn = pkgtempfn

            if pkgid < 0:  # assume a single socket machine
                self.coretemp[0] = cti
            else:
                self.coretemp[pkgid] = cti

    def readtempall(self):
        ctemp = self.coretemp
        ret = {}
        for pkgid in sorted(ctemp.keys()):
            temps = {}
            if os.access(ctemp[pkgid].pkgtempfn, os.R_OK):
                val = int(readbuf(ctemp[pkgid].pkgtempfn))/1000
                temps['pkg'] = val
            for c in sorted(ctemp[pkgid].coretempfns.keys()):
                if os.access(ctemp[pkgid].coretempfns[c], os.R_OK):
                    val = int(readbuf(ctemp[pkgid].coretempfns[c]))/1000
                    temps[c] = val
            ret[pkgid] = temps
        return ret

    def outputpercore(self, flag=True):
        self.percore = flag

    def sample(self):
        temp = self.readtempall()

        # constructing a json output
        ret = dict()
        for p in sorted(temp.keys()):
            key = "p%d" % p
            ret[key] = dict()
            pstat = self.getpkgstats(temp, p)
            ret[key]['mean'] = pstat[0]
            ret[key]['std'] = pstat[1]
            ret[key]['min'] = pstat[2]
            ret[key]['max'] = pstat[3]
            if self.percore:
                for c in sorted(temp[p].keys()):
                    ret[key][c] = temp[p][c]
        return ret

    def getmaxcoretemp(self, temps):
        vals = []
        for pkgid in temps.keys():
            for c in temps[pkgid].keys():
                vals.append(temps[pkgid][c])
        return np.max(vals)

    def getpkgstats(self, temps, pkgid):
        vals = []
        for c in temps[pkgid].keys():
            vals.append(temps[pkgid][c])
        return [np.mean(vals), np.std(vals), np.min(vals), np.max(vals)]

    def readpkgtemp(self):
        fn = "%s_input" % self.pkgtempfns[pkgid].pkgfn
        f = open(fn)
        v = int(f.readline())/1000.0
        f.close()
        return v

    def readcoretemp(self, pkgid):
        t = []
        for fnbase in self.pkgtempfns[pkgid].corefns:
            fn = "%s_input" % fnbase
            if not os.access(fn, os.R_OK):
                continue  # cpu may become offline
            f = open(fn)
            v = int(f.readline())/1000.0
            f.close()
            t.append(v)
        return t


class acpi_power_meter_reader:
    # add a nicer detection routine later
    def __init__(self):
        self.init = False
        fn = '/sys/class/hwmon/hwmon0/device/power1_average'
        if os.path.exists(fn):
            self.init = True

    def initialized(self):
        return self.init

    def read(self):
        if not self.init:
            return -1

        retval = -1

        fn = '/sys/class/hwmon/hwmon0/device/power1_average'
        try:
            f = open(fn, "r")
        except:
            return retval

        l = f.readline()
        retval = int(l) * 1e-6  # uW to W
        f.close()
        return retval

    def sample(self):
        if not self.init:
            return {}

        return {'power': self.read()}
