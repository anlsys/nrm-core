###############################################################################
# Copyright 2019 UChicago Argonne, LLC.
# (c.f. AUTHORS, LICENSE)
#
# This file is part of the NRM project.
# For more info, see https://xgitlab.cels.anl.gov/argo/nrm
#
# SPDX-License-Identifier: BSD-3-Clause
###############################################################################

from __future__ import print_function

from aci import ImageManifest
from yaml import load
from collections import namedtuple
import logging
from subprograms import ChrtClient, NodeOSClient, resources, SingularityClient
import operator

logger = logging.getLogger('nrm')
Container = namedtuple('Container', ['uuid', 'manifest', 'resources',
                                     'power', 'processes', 'clientids',
                                     'hwbindings'])


class ContainerManager(object):

    """Manages the creation, listing and deletion of containers, using a
    container runtime underneath."""

    def __init__(self, container_runtime, rm,
                 perfwrapper="nrm-perfwrapper",
                 linuxperf="perf",
                 pmpi_lib="/usr/lib/libnrm-pmpi.so",
                 downstream_event_uri="ipc:///tmp/nrm-downstream-event"):
        self.linuxperf = linuxperf
        self.perfwrapper = perfwrapper
        self.runtime = container_runtime
        self.containers = dict()
        self.pids = dict()
        self.resourcemanager = rm
        self.hwloc = rm.hwloc
        self.chrt = ChrtClient()
        self.pmpi_lib = pmpi_lib
        self.downstream_event_uri = downstream_event_uri

    def _get_container_tuple(self, container_name, manifest):
        """Retrieve a container tuple if the container exists, otherwise use
        the manifest to create a new one.

        Returns (bool, container_tuple), the first field telling if a container
        needs to be created."""

        if container_name in self.containers:
            return (False, self.containers[container_name])

        # ask the resource manager for resources
        ncpus = manifest.app['slice']['cpus']
        nmems = manifest.app['slice']['mems']
        req = resources(ncpus, nmems)
        allocated = self.resourcemanager.schedule(container_name, req)
        logger.info("create: allocation: %r", allocated)

        # Container power settings
        container_power = dict()
        container_power['profile'] = None
        container_power['policy'] = None
        container_power['damper'] = None
        container_power['slowdown'] = None
        container_power['manager'] = None

        if manifest.is_feature_enabled('power'):
            pp = manifest.app['power']
            if pp['profile'] is True:
                container_power['profile'] = dict()
                container_power['profile']['start'] = dict()
                container_power['profile']['end'] = dict()
            if pp['policy'] != "NONE":
                container_power['policy'] = pp['policy']
                container_power['damper'] = manifest.ratelimit
                container_power['slowdown'] = pp['slowdown']

        # Compute hardware bindings
        hwbindings = dict()
        if manifest.is_feature_enabled('hwbind'):
            hwbindings['distrib'] = sorted(self.hwloc.distrib(
                ncpus, allocated), key=operator.
                attrgetter('cpus'))
        return (True, Container(container_name, manifest, allocated,
                                container_power, {}, {}, hwbindings))

    def create(self, request):
        """Create a container according to the request.

        Returns the pid of the container or a negative number for errors."""

        manifestfile = request['manifest']
        command = request['file']
        args = request['args']
        environ = request['environ']
        container_name = request['uuid']
        logger.info("create: manifest file:  %s", manifestfile)
        logger.info("create: command:        %s", command)
        logger.info("create: args:           %r", args)
        logger.info("create: container name: %s", container_name)

        try:
            with open(manifestfile) as f:
                manifest = ImageManifest((load(f)))
        except Exception as e:
            logger.error("error occured in manifest loading:")
            raise(e)

        creation_needed, container = self._get_container_tuple(container_name,
                                                               manifest)
        if creation_needed:
            logger.info("Creating container %s", container_name)
            self.runtime.create(container, self.downstream_event_uri)
            self.containers[container_name] = container

        # build context to execute
        # environ['PATH'] = ("/usr/local/sbin:"
        #                   "/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin")
        environ['ARGO_CONTAINER_UUID'] = container_name
        environ['PERF'] = self.linuxperf
        environ['AC_APP_NAME'] = manifest['name']
        environ['AC_METADATA_URL'] = "localhost"

        # power profiling uses LD_PRELOAD, we use get to ensure that it
        # doesn't crash if the policy doesn't exits.
        if container.power.get('policy'):
            environ['LD_PRELOAD'] = self.pmpi_lib
            environ['NRM_TRANSMIT'] = '1'
            environ['ARGO_NRM_RATELIMIT'] = container.power['damper']

        # monitoring section involves libnrm
        if manifest.is_feature_enabled('monitoring'):
            environ['ARGO_NRM_RATELIMIT'] = \
                manifest.app['monitoring']['ratelimit']

        if container.power.get('policy') or \
                manifest.is_feature_enabled('monitoring'):
            environ['ARGO_NRM_DOWNSTREAM_EVENT_URI'] = \
                self.downstream_event_uri

        # build prefix to the entire command based on enabled features
        argv = []
        if manifest.is_feature_enabled('scheduler'):
            sched = manifest.app['scheduler']
            argv = self.chrt.getwrappedcmd(sched)

        # Use hwloc-bind to launch each process in the conatiner by prepending
        # it as an argument to the command line, if enabled in manifest.
        # The hardware binding computed using hwloc-distrib is used here
        # --single
        if container.hwbindings:
            # round robin over the cpu bindings available
            bind_index = len(container.processes) % \
                len(container.hwbindings['distrib'])
            argv.append('hwloc-bind')
            # argv.append('--single')
            cpumask = container.hwbindings['distrib'][bind_index].cpus[0]
            memmask = container.hwbindings['distrib'][bind_index].mems[0]
            logging.info('create: binding to: %s, %s', cpumask, memmask)
            argv.append("core:{}".format(cpumask))
            argv.append('--membind')
            argv.append("numa:{}".format(memmask))

        # It would've been better if argo-perf-wrapper wrapped around
        # argo-nodeos-config and not the final command -- that way it would
        # be running outside of the container.  However, because
        # argo-nodeos-config is suid root, perf can't monitor it.
        if manifest.is_feature_enabled('perfwrapper'):
            argv.append(self.perfwrapper)

        argv.append(command)
        argv.extend(args)

        # run my command
        process = self.runtime.execute(container_name, argv, environ)

        # register the process
        container.processes[process.pid] = process
        container.clientids[process.pid] = request['clientid']
        self.pids[process.pid] = container
        logger.info("Created process %s in container %s", process.pid,
                    container_name)
        return process.pid, container

    def delete(self, uuid):
        """Delete a container and kill all related processes."""
        self.runtime.delete(uuid, kill=True)
        self.resourcemanager.update(uuid)
        c = self.containers[uuid]
        del self.containers[uuid]
        map(lambda i: self.pids.pop(c.processes[i].pid, None), c.processes)

    def kill(self, uuid):
        """Kill all the processes of a container."""
        if uuid in self.containers:
            c = self.containers[uuid]
            logger.debug("killing %r:", c)
            for p in c.processes.values():
                try:
                    p.proc.terminate()
                except OSError:
                    logging.error("OS error: could not terminate process.")

    def list(self):
        """List the containers in the system."""
        return [{'uuid': c.uuid, 'pid': c.processes.keys()}
                for c in self.containers.values()]


class ContainerRuntime(object):

    """Implements the creation, deleting and spawning of commands for a
    container runtime."""

    def __init__(self):
        pass

    def create(self, container, downstream_uri):
        """Create the container defined by the container namedtuple on the
        system."""
        raise NotImplementedError

    def execute(self, container_uuid, args, environ):
        """Execute a command inside a container, using a similar interface to
        popen.

        Returns a tornado.process.Subprocess"""
        raise NotImplementedError

    def delete(self, container_uuid, kill=False):
        """Delete a container, possibly killing all the processes inside."""
        raise NotImplementedError


class NodeOSRuntime(ContainerRuntime):

    """Implements the container runtime interface using the nodeos
    subprogram."""

    def __init__(self, path="argo_nodeos_config"):
        """Creates the client for nodeos, with an optional custom
        path/command."""
        self.client = NodeOSClient(argo_nodeos_config=path)

    def create(self, container, downstream_uri):
        """Uses the container resource allocation to create a container."""
        self.client.create(container.uuid, container.resources)

    def execute(self, container_uuid, args, environ):
        """Launches a command in the container."""
        return self.client.execute(container_uuid, args, environ)

    def delete(self, container_uuid, kill=False):
        """Delete the container."""
        self.client.delete(container_uuid, kill)


class SingularityUserRuntime(ContainerRuntime):

    """Implements the container runtime interface using the singularity
    subprogram."""

    def __init__(self, path="singularity"):
        """Creates the client for singularity, with an optional custom
        path/command."""
        self.client = SingularityClient(singularity_path=path)

    def create(self, container, downstream_uri):
        """Uses the container resource allocation to create a container."""
        imageinfo = container.manifest.image
        self.client.instance_start(container.uuid, imageinfo['path'],
                                   [downstream_uri]+imageinfo['binds'])

    def execute(self, container_uuid, args, environ):
        """Launches a command in the container."""
        return self.client.execute(container_uuid, args, environ)

    def delete(self, container_uuid, kill=False):
        """Delete the container."""
        self.client.instance_stop(container_uuid, kill)


class DummyRuntime(ContainerRuntime):

    """Implements a dummy runtime that doesn't create any container, but still
    launches commands."""

    def __init__(self):
        pass

    def create(self, container, downstream_uri):
        pass

    def execute(self, container_uuid, args, environ):
        import tornado.process as process # type: ignore
        return process.Subprocess(args,
                                  stdout=process.Subprocess.STREAM,
                                  stderr=process.Subprocess.STREAM,
                                  env=environ)

    def delete(self, container_uuid, kill=False):
        pass
