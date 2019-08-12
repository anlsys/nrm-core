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
from functools import partial
import logging
import os
import signal
from zmq.eventloop import ioloop
from nrm.messaging import UpstreamRPCServer, UpstreamPubServer, DownstreamEventServer
from dataclasses import dataclass
from typing import Any

logger = logging.getLogger('nrm')


@dataclass
class Daemon(object):
    cfg: Any
    lib: Any

    def __post_init__(self):
        # create and register messaging servers
        self.upstream_pub_server = UpstreamPubServer(
            self.lib.upstreamPubAddress(self.cfg)
        )

        self.upstream_rpc_server = UpstreamRPCServer(
            self.lib.upstreamRpcAddress(self.cfg)
        )
        self.upstream_rpc_server.setup_recv_callback(self.do_upstream_receive)

        DownstreamEventServer(
            self.lib.downstreamEventAddress(self.cfg)
        ).setup_recv_callback(self.do_downstream_receive)

        # setup periodic sensor updates
        ioloop.PeriodicCallback(self.do_sensor, 1000).start()
        ioloop.PeriodicCallback(self.do_control, 1000).start()

        # take care of signals
        signal.signal(signal.SIGINT, self.do_signal)
        signal.signal(signal.SIGCHLD, self.do_signal)

        # starting the daemon
        ioloop.IOLoop.current().start()

    def do_downstream_receive(self, event, client):
        logger.info("receiving downstream message: %r", event)
        if event.tag == 'start':
            cid = event.container_uuid
            container = self.container_manager.containers[cid]
            self.application_manager.register(event, container)
        elif event.tag == 'progress':
            if event.application_uuid in self.application_manager.applications:
                app = self.application_manager.applications[
                    event.application_uuid]
                app.update_performance(event)
                # self.upstream_pub_server.send(event) TODO try this.
                self.upstream_pub_server.send(
                    tag='progress',
                    payload=event.payload,
                    application_uuid=event.application_uuid)
        elif event.tag == 'performance':
            if event.application_uuid in self.application_manager.applications:
                app = self.application_manager.applications[
                    event.application_uuid]
                app.update_performance(event)
            self.upstream_pub_server.send(
                tag='performance',
                payload=event.payload,
                container_uuid=event.container_uuid)
        elif event.tag == 'phasecontext':
            uuid = event.application_uuid
            if uuid in self.application_manager.applications:
                app = self.application_manager.applications[uuid]
                if bool(self.container_manager.containers):
                    cid = app.container_uuid
                    c = self.container_manager.containers[cid]
                    if c.power['policy']:
                        app.update_phase_context(event)
                        # Run container policy
                        self.controller.run_policy_container(c, app)
        elif event.tag == 'exit':
            uuid = event.application_uuid
            if uuid in self.application_manager.applications:
                self.application_manager.delete(uuid)
        else:
            logger.error("unknown msg: %r", event)
            return

    def do_upstream_receive(self, req, client):
        if req.tag == 'setPower':
            self.target = float(req.limit)
            logger.info("new target measure: %g", self.target)
            self.upstream_rpc_server.send(
                client,
                tag='getPower',
                limit=str(self.target))
        elif req.tag == 'run':
            logger.info("asked to run a command in a container: %r", req)
            container_uuid = req.container_uuid
            params = {'manifest': req.manifest,
                      'file': req.path,
                      'args': req.args,
                      'uuid': req.container_uuid,
                      'environ': req.environ,
                      'clientid': client,
                      }
            pid, container = self.container_manager.create(params)
            container_uuid = container.uuid
            if len(container.processes) == 1:
                # if container.power['policy']:
                    # container.power['manager'] = PowerPolicyManager(
                        # container.resources.cpus,
                        # container.power['policy'],
                        # float(container.power['damper']),
                        # float(container.power['slowdown']))
                # if container.power['profile']:
                    # p = container.power['profile']
                    # p['start'] = self.machine_info['energy']['energy']
                    # p['start']['time'] = self.machine_info['time']
                self.upstream_pub_server.send(
                    tag='start',
                    container_uuid=container_uuid,
                    errno=0 if container else -1,
                    power=container.power['policy'] or str(None))
            # now deal with the process itself
            self.upstream_rpc_server.send(
                client,
                tag='start',
                pid=pid,
                container_uuid=container_uuid)
            # setup io callbacks
            outcb = partial(self.do_children_io, client, container_uuid,
                            'stdout')
            errcb = partial(self.do_children_io, client, container_uuid,
                            'stderr')
            container.processes[pid].stdout.read_until_close(outcb, outcb)
            container.processes[pid].stderr.read_until_close(errcb, errcb)
        elif req.tag == 'kill':
            logger.info("asked to kill container: %r", req)
            response = self.container_manager.kill(req.container_uuid)
            # no update here, as it will trigger child exit
        elif req.tag == 'list':
            logger.info("asked for container list: %r", req)
            response = self.container_manager.list()
            self.upstream_rpc_server.send(
                client,
                tag="list",
                payload=response)
        else:
            logger.error("invalid command: %r", req.tag)

    def do_children_io(self, client, container_uuid, io, data):
        """Receive data from one of the children, and send it down the pipe.

        Meant to be partially defined on a children basis."""
        logger.info("%r received %r data: %r", container_uuid, io, data)
        self.upstream_rpc_server.send(
            client,
            tag=io,
            container_uuid=container_uuid,
            payload=data or 'eof')

    def do_sensor(self):
        self.machine_info = self.sensor_manager.do_update()
        logger.info("current state: %r", self.machine_info)
        try:
            total_power = self.machine_info['energy']['power']['total']
        except TypeError:
            logger.error("power sensor format malformed, "
                         "can not report power upstream.")
        else:
            self.upstream_pub_server.send(
                tag="power",
                total=total_power,
                limit=self.target)

    def do_control(self):
        plan = self.controller.planify(self.target, self.machine_info)
        action, actuator = plan
        if action:
            self.controller.execute(action, actuator)
            self.controller.update(action, actuator)
        # Call policy only if there are containers
        # if self.container_manager.containers:
            # self.controller.run_policy(self.container_manager.containers)

    def do_signal(self, signum, frame):
        if signum == signal.SIGINT:
            ioloop.IOLoop.current().add_callback_from_signal(self.do_shutdown)
        elif signum == signal.SIGCHLD:
            ioloop.IOLoop.current().add_callback_from_signal(self.do_children)
        else:
            logger.error("wrong signal: %d", signum)

    def do_children(self):
        # find out if children have terminated
        while True:
            try:
                pid, status, rusage = os.wait3(os.WNOHANG)
                if pid == 0 and status == 0:
                    break
            except OSError:
                break

            logger.info("child update %d: %r", pid, status)
            # check if its a pid we care about
            if pid in self.container_manager.pids:
                # check if this is an exit
                if os.WIFEXITED(status) or os.WIFSIGNALED(status):
                    container = self.container_manager.pids[pid]
                    clientid = container.clientids[pid]

                    # first, send a process_exit
                    self.upstream_rpc_server.send(
                        clientid,
                        tag="exit",
                        status=str(status),
                        container_uuid=container.uuid)
                    # Remove the pid of process that is finished
                    container.processes.pop(pid, None)
                    self.container_manager.pids.pop(pid, None)
                    logger.info("Process %s in Container %s has finished.",
                                pid, container.uuid)

                    # if this is the last process in the container,
                    # kill everything
                    if len(container.processes) == 0:
                        # deal with container exit
                        diff = {}
                        p = container.power
                        if p['policy']:
                            p['manager'].reset_all()
                        if p['profile']:
                            e = p['profile']['end']
                            self.machine_info = self.sensor_manager.do_update()
                            e = self.machine_info['energy']['energy']
                            e['time'] = self.machine_info['time']
                            s = p['profile']['start']
                            # Calculate difference between the values
                            diff = self.sensor_manager.calc_difference(s, e)
                            # Get final package temperature
                            temp = self.machine_info['temperature']
                            diff['temp'] = map(lambda k: temp[k]['pkg'], temp)
                            diff['policy'] = p['policy']
                            if p['policy']:
                                diff['damper'] = float(p['damper'])/1000000000
                                diff['slowdown'] = p['slowdown']
                            diff['nodename'] = self.sensor_manager.nodename
                            logger.info("Container %r profile data: %r",
                                        container.uuid, diff)
                        self.container_manager.delete(container.uuid)
                        self.upstream_pub_server.send(
                            tag="exit",
                            container_uuid=container.uuid,
                            profile_data=diff)
            else:
                logger.debug("child update ignored")
                pass

    def do_shutdown(self):
        ioloop.IOLoop.current().stop()


def runner(config, nrmlib):
    print(config)
    if nrmlib.verbose(config):
        logger.setLevel(logging.DEBUG)

    logfile = nrmlib.logfile(config)
    print("Logging to %s" % logfile)
    logger.addHandler(logging.FileHandler(logfile))

    Daemon(config, nrmlib)
