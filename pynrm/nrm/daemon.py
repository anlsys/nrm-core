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

        self.dispatch = {
            "noop": self.noop,
            "reply": self.reply
        }

        # logger.debug(self.lib.showState(t))
        # register messaging servers
        upstream_pub_a = self.lib.upstreamPubAddress(self.cfg)
        upstream_rpc_a = self.lib.upstreamRpcAddress(self.cfg)
        downstream_event_a = self.lib.downstreamEventAddress(self.cfg)

        self.upstream_pub = UpstreamPubServer(upstream_pub_a)
        self.upstream_rpc = UpstreamRPCServer(upstream_rpc_a)
        self.downstream_event = DownstreamEventServer(downstream_event_a)

        # register messaging server callbacks
        self.upstream_rpc.setup_recv_callback(
            self.wrap(self.lib.upstreamReceive))
        self.upstream_rpc.setup_recv_callback(
            self.wrap(self.lib.upstreamReceive))

        # setup periodic sensor updates
        # ioloop.PeriodicCallback(self.wrap(self.lib.doSensor), 10000).start()
        # ioloop.PeriodicCallback(self.wrap(self.lib.doControl), 10000).start()

        # take care of signals
        signal.signal(signal.SIGINT, self.do_signal)
        signal.signal(signal.SIGCHLD, self.do_signal)

        # starting the daemon
        ioloop.IOLoop.current().start()

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

    def wrap(self, f, *argsConfig, **kwargsConfig):
        def r(*argsCallback, **kwargsCallback):
            args = argsConfig + argsCallback
            kwargs = dict(kwargsConfig, ** kwargsCallback)
            # logger.debug(self.state)
            # logger.debug(args)
            st, bh = f(self.cfg, self.state, *args, **kwargs)
            self.state = st
            # logger.debug(bh)
            self.dispatch[bh[0]](bh[1:])
        return r

    def noop(self, args):
        pass

    def reply(self, args):
        self.upstream_rpc.send(*args)


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
