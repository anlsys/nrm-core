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
import tornado.process as process
# import tornado.ioloop
from zmq.eventloop import ioloop
from nrm.messaging import UpstreamRPCServer, UpstreamPubServer, DownstreamEventServer
from dataclasses import dataclass
import sys
from typing import Any

_logger = logging.getLogger('nrm')


@dataclass
class Daemon(object):
    cfg: Any
    lib: Any

    def __post_init__(self):
        self.state = self.lib.initialState(self.cfg)

        self.cmds = {}

        # register messaging servers
        upstream_pub_a = self.lib.upstreamPubAddress(self.cfg)
        upstream_rpc_a = self.lib.upstreamRpcAddress(self.cfg)
        downstream_event_a = self.lib.downstreamEventAddress(self.cfg)

        self.upstream_pub = UpstreamPubServer(upstream_pub_a)
        self.upstream_rpc = UpstreamRPCServer(upstream_rpc_a)
        self.downstream_event = DownstreamEventServer(downstream_event_a)

        self.dispatch = {
            "reply": self.upstream_rpc.send,
            "cmd": self.cmd,
            "kill": self.kill,
            "pop": self.popchild
        }

        # register messaging server callbacks
        self.upstream_rpc.setup_recv_callback(
            self.wrap("upstreamReceive"))
        self.downstream_event.setup_recv_callback(
            self.wrap("downstreamReceive"))

        # setup periodic sensor updates
        # ioloop.PeriodicCallback(self.wrap("doSensor"), 10000).start()
        # ioloop.PeriodicCallback(self.wrap("doControl"), 10000).start()

        # take care of signals
        signal.signal(signal.SIGINT, self.do_signal)
        signal.signal(signal.SIGCHLD, self.do_signal)

        # starting the daemon

        ioloop.IOLoop.current().start()

    def do_signal(self, signum, frame):
        """
            'do_signal' installs interruption and children death handler in the current
            ioloop round.
        """
        if signum == signal.SIGINT:
            ioloop.IOLoop.current().add_callback_from_signal(self.do_shutdown)
        elif signum == signal.SIGCHLD:
            ioloop.IOLoop.current().add_callback_from_signal(self.do_children)
        else:
            _logger.error("Unhandled signal: %d", signum)

    def do_children(self):
        # find out if children have terminated
        while True:
            try:
                pid, status, rusage = os.wait3(os.WNOHANG)
                if pid == 0 and status == 0:
                    break
                self.wrap("childDied")(pid, status)
            except OSError:
                break
        pass

    def do_shutdown(self):
        self.wrap("doShutdown")()
        ioloop.IOLoop.current().stop()

    def wrap(self, name, *argsConfig, **kwargsConfig):
        """
            'wrap' is a decorator that turns a shared library symbol into a
            behavior-reacting function. It can process any symbol whose
            underlying unpacked type is:
                Cfg -> NrmState -> <...> -> IO (Cfg, NrmState)
            where <...> can be any number of arguments.
        """

        def r(*argsCallback, **kwargsCallback):
            args = argsConfig + argsCallback
            _logger.debug(
                "calling into nrm.so with symbol %s and arguments %s", name, str(args))
            kwargs = dict(kwargsConfig, ** kwargsCallback)
            st, bh = self.lib.__getattr__(name)(
                self.cfg, self.state, *args, **kwargs)
            self.state = st
            _logger.debug("received behavior from nrm.so: %s", str(bh))
            if bh != "noop":
                self.dispatch[bh[0]](*bh[1:])
        return r

    def cmd(self, cmdID, cmd, arguments, environment):
        """
            start a runtime subprocess and handles start/failure by registering
            its cmdID with the state in the appropriate manner.
        """
        registerSuccess = self.wrap("registerCmdSuccess", cmdID)
        registerFailed = self.wrap("registerCmdFailure")
        environment = dict(environment)
        _logger.debug("starting command " + str(cmd) + " with argument list "
                      + str(arguments))
        try:
            p = process.Subprocess([cmd] + arguments,
                                   stdout=process.Subprocess.STREAM,
                                   stderr=process.Subprocess.STREAM,
                                   close_fds=True,
                                   env=environment,
                                   cwd=environment['PWD'])
            outcb = self.wrap("doStdout", cmdID.encode())
            errcb = self.wrap("doStderr", cmdID.encode())
            p.stdout.read_until_close(outcb, outcb)
            p.stderr.read_until_close(errcb, errcb)
            self.cmds[cmdID] = p
            registerSuccess(p.proc.pid)
            _logger.debug("Command start success.")
        except FileNotFoundError as e:
            registerFailed()
            _logger.debug("Command start failure.")
            raise e

    def kill(self, cmdIDs, messages):
        """
            kill children and send messages up
        """
        _logger.debug("Killing children: %s", str(cmdIDs))
        for cmdID in cmdIDs:
            if cmdID in self.cmds.keys():
                self.cmds.pop(cmdID).proc.terminate()
        for m in messages:
            self.upstream_rpc.send(*m)

    def popchild(self, cmdIDs, messages):
        """
            pop child child and send a message up
        """
        _logger.debug("Killing children: %s", str(cmdIDs))
        for cmdID in cmdIDs:
            if cmdID in self.cmds.keys():
                self.cmds.pop(cmdID).proc.terminate()
        assert(len(messages) == 1)
        for m in messages:
            self.upstream_rpc.send(*m)


def runner(config, lib):
    logfile = lib.logfile(config)
    print("Logging to %s" % logfile)
    _logger.addHandler(logging.FileHandler(logfile))

    if lib.isVerbose(config):
        _logger.info("Setting configuration to INFO level.")
        _logger.setLevel(logging.INFO)

    if lib.isDebug(config):
        _logger.info(
            "Setting configuration to DEBUG level and redirecting to stdout.")
        _logger.setLevel(logging.DEBUG)
        _logger.debug("NRM Daemon configuration:")
        _logger.debug(lib.showConfiguration(config))

    Daemon(config, lib)
