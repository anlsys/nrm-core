###############################################################################
# Copyright 2019 UChicago Argonne, LLC.
# (c.f. AUTHORS, LICENSE)
#
# This file is part of the NRM project.
# For more info, see https://xgitlab.cels.anl.gov/argo/nrm
#
# SPDX-License-Identifier: BSD-3-Clause
###############################################################################

"""Various clients for system utilities."""
import collections
import logging
import xml.etree.ElementTree
import tornado.process as process # type: ignore
import subprocess

logger = logging.getLogger('nrm')
resources = collections.namedtuple("Resources", ["cpus", "mems"])


def logpopen(p, args, stdout, stderr):
    """log popen cmd."""
    logger.debug("popen cmd: %r", args)
    logger.debug("popen return code: %s", p.returncode)
    logger.debug("popen stdout: %r", stdout)
    logger.debug("popen, stderr: %r", stderr)


def bitmask2list(mask):
    """Convert a bitmask to the list of power of 2 set to 1."""
    i = int(mask or '0x0', base=16)
    ret = []
    for j in range(i.bit_length()):
        m = 1 << j
        if (i & m):
            ret.append(j)
    return ret


def list2bitmask(l):
    """Convert a list into a bitmask."""
    m = 0
    for e in l:
        m |= 1 << int(e)
    return hex(m)


class NodeOSClient(object):

    """Client to argo_nodeos_config."""

    def __init__(self, argo_nodeos_config="argo_nodeos_config"):
        """Load client configuration."""
        self.prefix = argo_nodeos_config

    def getavailable(self):
        """Gather available resources."""
        args = [self.prefix, "--show_available_resources=shared:false"]
        p = subprocess.Popen(args, stdout=subprocess.PIPE,
                             stderr=subprocess.PIPE)
        stdout, stderr = p.communicate()
        logpopen(p, args, stdout, stderr)
        # parse the format: first line is threads, then a list as multiline,
        # then nodes, and the same
        cpus = []
        mems = []
        lines = stdout.splitlines()
        splitindex = lines.index('------------Memory nodes------------')
        cpuslines = lines[1:splitindex]
        memlines = lines[splitindex+1:]
        for l in cpuslines:
            cpus.extend(l.split())
        for l in memlines:
            mems.extend(l.split())
        return resources([int(x) for x in cpus], [int(x) for x in mems])

    def create(self, name, params):
        """Create container, according to params."""
        args = [self.prefix]
        cmd = "--create_container="
        cmd += 'name:{0}'.format(name)
        cmd += ' cpus:[{0}]'.format(",".join([str(x) for x in params.cpus]))
        cmd += ' mems:[{0}]'.format(",".join([str(x) for x in params.mems]))
        args.append(cmd)
        try:
            p = subprocess.Popen(args, stdout=subprocess.PIPE,
                                 stderr=subprocess.PIPE)
        except OSError:
            logger.error("Could not run argo_nodeos_config. Use either the"
                         "--argo_nodeos_config nrmd option or the"
                         " ARGO_NODEOS_CONFIG env var. to configure it.")

        stdout, stderr = p.communicate()
        logpopen(p, args, stdout, stderr)

    def attach(self, name, pid):
        """Attach a pid to a container."""
        args = [self.prefix]
        cmd = '--attach_to_container='
        cmd += 'name:{0}'.format(name)
        cmd += ' pids:[{0}]'.format(pid)
        args.append(cmd)
        p = subprocess.Popen(args, stdout=subprocess.PIPE,
                             stderr=subprocess.PIPE)
        stdout, stderr = p.communicate()
        logpopen(p, args, stdout, stderr)

    def delete(self, name, kill=False):
        """Destroy container."""
        # destroy container
        args = [self.prefix]
        cmd = '--delete_container='
        cmd += 'name:{0}'.format(name)
        if kill:
            cmd += ' kill_content:true'
        args.append(cmd)
        p = subprocess.Popen(args, stdout=subprocess.PIPE,
                             stderr=subprocess.PIPE)
        stdout, stderr = p.communicate()
        logpopen(p, args, stdout, stderr)

    def execute(self, name, argv, environ):
        """Execute argv inside container."""
        args = [self.prefix]
        cmd = '--exec='
        cmd += 'name:{0}'.format(name)
        # argo_nodeos_config takes argv as 'arg0 arg1 ...' so we need to merge
        # the arguments into a single list, with single quotes. We also need
        # to escape spaces from arguments before.
        argv = [s.replace(' ', r'\ ') for s in argv]
        cmd += " argv:'"+" ".join(argv)+"'"
        env = ['{0}={1}'.format(envname, envval.replace(' ', r'\ '))
               for envname, envval in environ.items()]
        cmd += " env:'"+" ".join(env)+"'"
        args.append(cmd)
        return process.Subprocess(args,
                                  stdout=process.Subprocess.STREAM,
                                  stderr=process.Subprocess.STREAM,
                                  close_fds=True,
                                  cwd=environ['PWD'])


class SingularityClient(object):

    """Client to singularity."""

    def __init__(self, singularity_path="singularity"):
        """Load client configuration."""
        self.prefix = singularity_path

    def instance_start(self, instance_name, container_image, bind_list=[]):
        """Start a named instance of a container image.

        Note that singularity will also start the startscript if
        defined in the container image, which might be an issue."""
        args = [self.prefix]
        args.extend(['instance', 'start'])
        if bind_list:
            args.extend(['--bind', ','.join(bind_list)])
        args.extend([container_image, instance_name])
        logger.error("launching singularity command: %s", args)
        p = subprocess.Popen(args, stdout=subprocess.PIPE,
                             stderr=subprocess.PIPE)
        stdout, stderr = p.communicate()
        logpopen(p, args, stdout, stderr)

    def execute(self, instance_name, argv, environ):
        """Execute argv inside container.

        singularity exec instance://instance_name <command>"""
        args = [self.prefix]  # singularity
        container_name = "instance://" + instance_name
        args.extend(['exec', container_name])
        args.extend(argv)
        return process.Subprocess(args, env=environ,
                                  stdout=process.Subprocess.STREAM,
                                  stderr=process.Subprocess.STREAM,
                                  close_fds=True,
                                  cwd=environ['PWD'])

    def instance_stop(self, instance_name, kill=False):
        """Stop an instance and kill everything in it."""

        args = [self.prefix]
        args.extend(['instance', 'stop'])
        if kill:
            args.append("--force")
        args.append(instance_name)
        p = subprocess.Popen(args, stdout=subprocess.PIPE,
                             stderr=subprocess.PIPE)
        stdout, stderr = p.communicate()
        logpopen(p, args, stdout, stderr)


class ChrtClient(object):

    """Client to chrt command line wrapper."""

    flags = {'SCHED_OTHER': '--other',
             'SCHED_BATCH': '--batch',
             'SCHED_FIFO': '--fifo',
             'SCHED_IDLE': '--idle',
             'SCHED_RR': '--rr',
             'SCHED_HPC': '--hpc'
             }

    def __init__(self):
        """Load configuration."""
        self.prefix = "chrt"

    def getwrappedcmd(self, params):
        """Return a list of args to prepend to a popen call."""
        args = [self.prefix]
        args.append(self.flags[params.policy])
        args.append(params.priority)
        return args


class HwlocClient(object):

    """Client to hwloc binaries."""

    def __init__(self, hwloc="hwloc"):
        """Load configuration."""
        self.prefix = hwloc

    def info(self):
        """Return list of all cpus and mems."""
        cmd = self.prefix + "-ls"
        args = [cmd, '--whole-system', '--output-format', 'xml']
        p = subprocess.Popen(args, stdout=subprocess.PIPE,
                             stderr=subprocess.PIPE)
        stdout, stderr = p.communicate()
        logpopen(p, args, stdout, stderr)
        xmlroot = xml.etree.ElementTree.fromstring(stdout)
        ret = resources([], [])
        for obj in xmlroot.iter('object'):
            if obj.attrib['type'] == "NUMANode":
                ret.mems.append(int(obj.attrib['os_index']))
            if obj.attrib['type'] == "PU":
                ret.cpus.append(int(obj.attrib['os_index']))
        # if there's only one memory node, hwloc doesn't list it
        if not ret.mems:
            ret.mems.append(0)
        return ret

    def all2fake(self, resources):
        """Convert resource description of the system into fake topology.

        We need that because hwloc barfs on fake numa nodes.
        """
        # easy version: we have as many numa nodes as we have cores
        mems = len(resources.mems)
        cpus = len(resources.mems)
        assert cpus % mems == 0
        pu = cpus // mems
        return "numa: %s pu:%s".format(mems, pu)

    def distrib(self, numprocs, restrict=None, fake=None):
        """Distribute numprocs across the hierarchy."""
        # The original command only reports back cpusets. We do better, by
        # reporting the mems that go with it. This requires some magic, using
        # hwloc-ls to find the numa node associated with a cpuset reported by
        # distrib
        allresources = self.info()
        cmd = [self.prefix + "-distrib"]
        if fake:
            cmd.extend(['-i', self.all2fake(fake)])
        args = cmd + ["--whole-system", "--taskset", str(numprocs)]
        if restrict:
            mask = list2bitmask(restrict.cpus)
            args.extend(['--restrict', mask])
        p = subprocess.Popen(args, stdout=subprocess.PIPE,
                             stderr=subprocess.PIPE)
        stdout, stderr = p.communicate()
        logpopen(p, args, stdout, stderr)
        cpusets = stdout.splitlines()
        dret = {}
        for c in cpusets:
            dret[c] = resources(bitmask2list(c), [])

        # list all resources, and display cpusets too
        # this will give us the memories associated with each cpuset.
        cmd = [self.prefix + "-ls"]
        if fake:
            cmd.extend(['-i', self.all2fake(fake)])
        args = cmd + ["--whole-system", "-c", "--taskset"]
        p = subprocess.Popen(args, stdout=subprocess.PIPE,
                             stderr=subprocess.PIPE)
        stdout, stderr = p.communicate()
        logpopen(p, args, stdout, stderr)
        lines = stdout.splitlines()
        curmem = allresources.mems
        for l in lines:
            pos = l.find('cpuset=')
            if pos != -1:
                c = l[l.find('cpuset='):].lstrip('cpuset=')
                numa = l.find('NUMANode')
                cset = set(bitmask2list(c))
                if numa != -1:
                    uid = int(l.split()[1].lstrip('L#'))
                    curmem = [uid]
                for mask in dret:
                    cs = set(bitmask2list(mask))
                    if cset.issubset(cs):
                        dret[mask].mems.extend(curmem)
        # At this point, we have valid cpusets, but the mems associated are not
        # restricted, and not necessarily the right amount. We need to:
        #    - remove memories for the restricted set
        #    - split each (cpuset, mems) that is too big into a list of memset
        #    choices of the right size
        return dret.values()
