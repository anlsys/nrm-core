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
import sys
from typing import Any

logger = logging.getLogger('nrm')


@dataclass
class Daemon(object):
    cfg: Any
    lib: Any

    def __post_init__(self):
        self.state = self.lib.initialState(self.cfg)
        # register messaging servers
        upstream_pub_a = self.lib.upstreamPubAddress(self.cfg)
        upstream_rpc_a = self.lib.upstreamRpcAddress(self.cfg)
        downstream_event_a = self.lib.downstreamEventAddress(self.cfg)

        self.upstream_pub = UpstreamPubServer(upstream_pub_a)
        self.upstream_rpc = UpstreamRPCServer(upstream_rpc_a)
        self.downstream_event = DownstreamEventServer(downstream_event_a)

        # register messaging server callbacks
        self.upstream_rpc.setup_recv_callback(
            self.behave(self.lib.upstreamReceive))
        self.downstream_event.setup_recv_callback(
            self.behave(self.lib.downstreamReceive))

        # setup periodic sensor updates
        ioloop.PeriodicCallback(self.behave(self.lib.doSensor), 1000).start()
        ioloop.PeriodicCallback(self.behave(self.lib.doControl), 1000).start()

        # take care of signals
        signal.signal(signal.SIGINT, self.do_signal)
        signal.signal(signal.SIGCHLD, self.do_signal)

        # starting the daemon
        ioloop.IOLoop.current().start()

    def do_downstream_receive(self, event, client):
        logger.info(
            "receiving downstream message: %r from client %s",
            event, str(client)
        )
        pass

    def do_upstream_receive(self, req, client):
        logger.info(
            "receiving upstream message: %r from client %s",
            req, str(client)
        )
        pass

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
        pass

    def do_shutdown(self):
        ioloop.IOLoop.current().stop()

    def behave(self, f, **kwargsConfig):
        def r(**kwargsCallback):
            st, bh = f(self.state, **kwargsConfig, **kwargsCallback)
            self.state = st
            logger.debug(self.lib.showState(st))
            logger.debug(bh)
        return r


def behave(behavior):
    pass


def runner(config, lib):
    logfile = lib.logfile(config)
    print("Logging to %s" % logfile)
    logger.addHandler(logging.FileHandler(logfile))

    if lib.isVerbose(config):
        logger.info("Setting configuration to INFO level.")
        logger.setLevel(logging.INFO)

    if lib.isDebug(config):
        logger.info(
            "Setting configuration to DEBUG level and redirecting to stdout.")
        logger.setLevel(logging.DEBUG)
        logger.addHandler(logging.StreamHandler(sys.stdout))
        logger.debug("NRM Daemon configuration:")
        logger.debug(lib.showConfiguration(config))

    Daemon(config, lib)
