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

import logging

logger = logging.getLogger('nrm')


class Action(object):

    """Information about a control action."""

    def __init__(self, target, command, delta):
        self.target = target
        self.command = command
        self.delta = delta


# class ApplicationActuator(object):
#
#    """Actuator in charge of application thread control."""
#
#    def __init__(self, am, pubstream):
#        self.application_manager = am
#        self.pubstream = pubstream
#
#    def available_actions(self, target):
#        ret = []
#        for identity, application in \
#                self.application_manager.applications.iteritems():
#            if target in application.get_allowed_thread_requests():
#                delta = application.get_thread_request_impact(target)
#                ret.append(Action(application, target, delta))
#        return ret
#
#    def execute(self, action):
#        target_threads = action.target.threads
#        update = {'type': 'application',
#                  'command': 'threads',
#                  'uuid': action.target.uuid,
#                  'event': 'threads',
#                  }
#        if action.command == 'i':
#            payload = target_threads['cur'] + 1
#        elif action.command == 'd':
#            payload = target_threads['cur'] - 1
#        else:
#            assert False, "impossible command"
#        update['payload'] = payload
#        self.pubstream.send_json()
#
#    def update(self, action):
#        action.target.do_thread_transition(action.command)


class PowerActuator(object):

    """Actuator in charge of power control."""

    def __init__(self, sm):
        self.sensor_manager = sm

    def available_actions(self, target):
        actions = []
        pl = self.sensor_manager.get_powerlimits()
        logger.info("power limits: %r:", pl)
        if target == 'i':
            for k in pl:
                r = range(int(pl[k]['curW'])+1, int(pl[k]['maxW']))
                actions.extend([Action(k, s, s - r[0]) for s in r])
        elif target == 'd':
            for k in pl:
                r = range(1, int(pl[k]['curW']))
                actions.extend([Action(k, s, r[-1] - s) for s in r])
        return actions

    def execute(self, action):
        logger.info("changing power limit: %r, %r", action.command,
                    action.delta)
        self.sensor_manager.set_powerlimit(action.target, action.command)

    def update(self, action):
        pass


class Controller(object):

    """Implements a control loop for resource management."""

    def __init__(self, actuators):
        self.actuators = actuators

    def planify(self, target, machineinfo):
        """Plan the next action for the control loop."""
        try:
            total_power = machineinfo['energy']['power']['total']
        except TypeError:
            logging.error("\"machineinfo\" malformed. Can not run "
                          "control loop.")
            return (None, None)

        direction = None
        if total_power < target:
            direction = 'i'
        elif total_power > target:
            direction = 'd'

        if direction:
            actions = []
            for act in self.actuators:
                newactions = act.available_actions(direction)
                actions.extend([(a, act) for a in newactions])
            if actions:
                # TODO: better choice
                actions.sort(key=lambda x: x[0].delta)
                return actions.pop(0)
            else:
                return (None, None)

    def execute(self, action, actuator):
        """Build the action for the appropriate manager."""
        actuator.execute(action)

    def update(self, action, actuator):
        """Update tracking across the board to reflect the last action."""
        actuator.update(action)

    def run_policy_container(self, container, application):
        """Run policies on a container."""
        ids = container.resources.cpus
        pcs = application.phase_contexts
        # Run policy only if all phase contexts have been received
        if not filter(lambda i: not pcs[i]['set'], ids):
            # Only run policy if all phase contexts are an
            # aggregation of same number of phases
            aggs = [pcs[i]['aggregation'] for i in ids]
            if aggs.count(aggs[0]) == len(aggs):
                container.power['manager'].run_policy(pcs)
                if filter(lambda i: pcs[i]['set'], ids):
                    logger.debug("Phase context not reset %r", application)
            else:
                container.power['manager'].reset_all()
                for i in ids:
                    pcs[i]['set'] = False

    def run_policy(self, containers):
        """Run policies on containers with policies set."""
        for container in containers:
            p = containers[container].power
            if p['policy']:
                apps = self.actuators[0].application_manager.applications
                if apps:
                    app = next(apps[a] for a in apps if apps[a].container_uuid
                               == container)
                    self.run_policy_container(containers[container], app)
