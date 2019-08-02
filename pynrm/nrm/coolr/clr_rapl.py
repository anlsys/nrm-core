#!/usr/bin/env python
#
# coolr rapl related codes
#
# This code requires the intel_powerclamp module.
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


import os
import sys
import re
import time
import getopt


class rapl_reader:
    dryrun = False

    rapldir = '/sys/devices/virtual/powercap/intel-rapl'

    re_domain = re.compile('package-([0-9]+)(/\S+)?')

    def readint(self, fn):
        v = -1
        for retry in range(0, 10):
            try:
                f = open(fn)
                v = int(f.readline())
                f.close()
            except:
                continue
        return v

    def writeint(self, fn, v):
        ret = False
        try:
            f = open(fn, 'w')
            f.write('%d' % v)
            f.close()
            ret = True
        except:
            ret = False
        return ret
    #
    # e.g.,
    # intel-rapl:0/name
    # intel-rapl:0/intel-rapl:0:0/name
    # intel-rapl:0/intel-rapl:0:1/name

    def __init__(self):
        self.dirs = {}
        self.max_energy_range_uj_d = {}

        if self.dryrun:
            return

        self.init = False
        if not os.path.exists(self.rapldir):
            return
        self.init = True

        for d1 in os.listdir(self.rapldir):
            dn = "%s/%s" % (self.rapldir, d1)
            fn = dn + "/name"
            if os.access(fn, os.R_OK):
                f = open(fn)
                l = f.readline().strip()
                f.close()
                if re.search('package-[0-9]+', l):
                    self.dirs[l] = dn
                    pkg = l
                    for d2 in os.listdir("%s/%s" % (self.rapldir, d1)):
                        dn = "%s/%s/%s" % (self.rapldir, d1, d2)
                        fn = dn + "/name"
                        if os.access(fn, os.R_OK):
                            f = open(fn)
                            l = f.readline().strip()
                            f.close
                            if re.search('core|dram', l):
                                self.dirs['%s/%s' % (pkg, l)] = dn

        for k in sorted(self.dirs.keys()):
            fn = self.dirs[k] + "/max_energy_range_uj"
            try:
                f = open(fn)
            except:
                print('Unable to open', fn)
                sys.exit(0)
            self.max_energy_range_uj_d[k] = int(f.readline())
            f.close()

        self.start_energy_counter()

    def initialized(self):
        return self.init

    def shortenkey(self, str):
        return str.replace('package-', 'p')

    #   for k in sorted(self.dirs.keys()):
    #      print k, self.max_energy_range_uj_d[k]

    def readenergy(self):
        if not self.init:
            return

        ret = {}
        ret['time'] = time.time()
        if self.dryrun:
            ret['package-0'] = readuptime()*1000.0*1000.0
            return ret
        for k in sorted(self.dirs.keys()):
            fn = self.dirs[k] + "/energy_uj"
            ret[k] = self.readint(fn)
        return ret

    # Read all possible power caps, except package 'short_term', which
    # will be supported later. This function is designed to be called
    # from a slow path. return a dict with long domain names as keys
    # and a value contains a dict with 'curW', 'maxW', 'enabled'

    def readpowerlimitall(self):
        if not self.init:
            return

        ret = {}
        if self.dryrun:
            ret['package-0'] = 100.0
            return ret
        for k in sorted(self.dirs.keys()):
            dvals = {}
            v = self.readint(self.dirs[k] + '/constraint_0_power_limit_uw')
            dvals['curW'] = v * 1e-6  # uw to w

            v = self.readint(self.dirs[k] + '/constraint_0_max_power_uw')
            dvals['maxW'] = v * 1e-6  # uw to w

            dvals['enabled'] = False
            v = self.readint(self.dirs[k] + '/enabled')
            if v == 1:
                dvals['enabled'] = True
            ret[k] = dvals
        return ret

    def diffenergy(self, e1, e2):  # e1 is prev and e2 is not
        ret = {}
        ret['time'] = e2['time'] - e1['time']
        for k in self.max_energy_range_uj_d:
            if e2[k] >= e1[k]:
                ret[k] = e2[k] - e1[k]
            else:
                ret[k] = (self.max_energy_range_uj_d[k]-e1[k]) + e2[k]
        return ret

    # calculate the average power from two energy values
    # e1 and e2 are the value returned from readenergy()
    # e1 should be sampled before e2
    def calcpower(self, e1, e2):
        ret = {}
        delta = e2['time'] - e1['time']  # assume 'time' never wrap around
        ret['delta'] = delta
        if self.dryrun:
            k = 'package-0'
            ret[k] = e2[k] - e1[k]
            ret[k] /= (1000.0*1000.0)  # conv. [uW] to [W]
            return ret

        for k in self.max_energy_range_uj_d:
            if e2[k] >= e1[k]:
                ret[k] = e2[k] - e1[k]
            else:
                ret[k] = (self.max_energy_range_uj_d[k]-e1[k]) + e2[k]
            ret[k] /= delta
            ret[k] /= (1000.0*1000.0)  # conv. [uW] to [W]
        return ret

    # this should be renamed to reset_...
    def start_energy_counter(self):
        if not self.initialized():
            return

        self.start_time_e = time.time()
        self.totalenergy = {}
        self.lastpower = {}

        e = self.readenergy()
        for k in sorted(e.keys()):
            if k != 'time':
                self.totalenergy[k] = 0.0
                self.lastpower[k] = 0.0
        self.prev_e = e

    # XXX: fix the total energy tracking later
    def read_energy_acc(self):
        if not self.initialized():
            return

        e = self.readenergy()

        de = self.diffenergy(self.prev_e, e)

        for k in sorted(e.keys()):
            if k != 'time':
                self.totalenergy[k] += de[k]
                self.lastpower[k] = de[k]/de['time']/1000.0/1000.0
        self.prev_e = e

        return e

    def stop_energy_counter(self):
        if not self.initialized():
            return

        e = self.read_energy_acc()
        self.stop_time = time.time()

    def sample(self, accflag=False):
        if not self.initialized():
            return

        e = self.readenergy()

        de = self.diffenergy(self.prev_e, e)

        for k in sorted(e.keys()):
            if k != 'time':
                if accflag:
                    self.totalenergy[k] += de[k]
                self.lastpower[k] = de[k]/de['time']/1000.0/1000.0
        self.prev_e = e

        ret = dict()
        ret['energy'] = dict()
        for k in sorted(e.keys()):
            if k != 'time':
                ret['energy'][self.shortenkey(k)] = e[k]

        ret['power'] = dict()
        totalpower = 0.0
        for k in sorted(self.lastpower.keys()):
            if k != 'time':
                ret['power'][self.shortenkey(k)] = self.lastpower[k]
                # this is a bit ad hoc way to calculate the total.
                # needs to be fixed later
                if k.find("core") == -1:
                    totalpower += self.lastpower[k]
        ret['power']['total'] = totalpower

        ret['powercap'] = dict()
        rlimit = self.readpowerlimitall()
        for k in sorted(rlimit.keys()):
            ret['powercap'][self.shortenkey(k)] = rlimit[k]['curW']

        return ret

    def total_energy_json(self):
        if not self.initialized():
            return ''

        dt = self.stop_time - self.start_time_e
        # constructing a json output
        e = self.totalenergy
        s = '{"total":"energy","difftime":%f' % (dt)
        for k in sorted(e.keys()):
            if k != 'time':
                s += ',"%s":%d' % (self.shortenkey(k), e[k])
        s += '}'
        return s

    def conv_long2short(self, n):
        m = self.re_domain.match(n)
        sn = ''
        if m:
            pkgid = int(m.group(1))
            sn = 'p%d' % (pkgid)

            if m.group(2):
                subname = m.group(2)[1:]
                sn += subname
        return sn

    #
    # APIs for power capping
    #

    def get_powerdomains(self):
        return self.readpowerlimitall().keys

    def get_powerlimits(self):
        return self.readpowerlimitall()

    def _set_powerlimit(self, rrdir, newval, id=0):
        fn = rrdir + '/constraint_%d_power_limit_uw' % id
        uw = newval * 1e6
        try:
            f = open(fn, 'w')
        except:
            print('Failed to update:', fn, '(root privilege is required)')
            return
        f.write('%d' % uw)
        f.close()

    def set_powerlimit(self, newval, dom):
        l = self.dirs[dom]
        self._set_powerlimit(l, newval)

    def set_powerlimit_pkg(self, newval):
        rlims = self.readpowerlimitall()
        for k in rlims.keys():
            if re.findall('package-[0-9]$', k):
                self._set_powerlimit(self.dirs[k], newval)

    # Implement the following method
    # is_enabled_rapl(), enable_rapl(), disable_rapl()
    # convshort2long
    # pkgid2cpuids, cpuid2pkgid


def usage():
    print('clr_rapl.py [options]')
    print('')
    print('--show [pkgid]:   show the current setting')
    print('--limitp val: set the limit to all packages')
    print('         [pkgid:]powerval e.g., 140, 1:120')
    print('')
    print('If no option is specified, run the test pattern.')
    print('')


def test_conv():
    l = ['package-1', 'package-3/dram',  'package-2/core']

    for s in l:
        rr.conv_long2short(s)


def report_powerlimits():
    l = rr.get_powerlimits()
    for k in l.keys():
        if l[k]['enabled']:
            print(k, 'curW:', l[k]['curW'], 'maxW:', l[k]['maxW'])


def run_powercap_testbench():
    # hard-coded for Haswell E5-2699v2 dual socket
    print(rr.get_powerdomains())
    report_powerlimits()

    w = 10
    rr.set_powerlimit_pkg(120)
    time.sleep(w)
    rr.set_powerlimit_pkg(80)
    time.sleep(w)
    rr.set_powerlimit(130, 'package-1')
    time.sleep(w)
    rr.set_powerlimit_pkg(145)


if __name__ == '__main__':
    rr = rapl_reader()

    if not rr.initialized():
        print('Error: No intel rapl sysfs found')
        sys.exit(1)

    shortopt = "h"
    longopt = ['getpd', 'getplim', 'setplim=', 'show', 'limitp=', 'testbench']
    try:
        opts, args = getopt.getopt(sys.argv[1:],
                                   shortopt, longopt)

    except getopt.GetoptError as err:
        print(err)
        usage()
        sys.exit(1)

    for o, a in opts:
        if o in ('-h'):
            usage()
            sys.exit(0)
        elif o in ("--testbench"):
            print('Start: testbench')
            run_powercap_testbench()
            print('Stop: testbench')
            sys.exit(0)
        elif o in ("--getpd"):
            print(rr.get_powerdomains())
            sys.exit(0)
        elif o in ("--getplim", "--show"):
            report_powerlimits()
            sys.exit(0)
        elif o in ("--setplim", "--limitp"):
            v = float(a)
            rr.set_powerlimit_pkg(v)
            report_powerlimits()
            sys.exit(0)

    rr.start_energy_counter()
    for i in range(0, 3):
        time.sleep(1)
        s = rr.sample_and_json(accflag=True)
        print(s)
    rr.stop_energy_counter()
    s = rr.total_energy_json()
    print(s)

    sys.exit(0)
