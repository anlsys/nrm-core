###############################################################################
# Copyright 2019 UChicago Argonne, LLC.
# (c.f. AUTHORS, LICENSE)
#
# This file is part of the NRM project.
# For more info, see https://xgitlab.cels.anl.gov/argo/nrm
#
# SPDX-License-Identifier: BSD-3-Clause
###############################################################################

""" Power Policy Module:
    This module provides the interfaces that enable use of policies to control
    processor power using controls available in the processor.
    E.g. Dynamic Duty Cycle Modulation (DDCM), Dynamic Voltage
    and Frequency Scaling (DVFS) and Power capping

    The policies target problems like workload imbalance, memory saturation
    seen very often in parallel applications.

    To mitigate workload imbalance the policies adapt core frequencies to
    workload characteristics through use of core-specific power controls.
    The user can choose from three policies - DDCM, DVFS and a combination of
    DVFS and DDCM to mitiage  workload imbalance in parallel applications that
    use barrier synchronizations.
    The effective frequency of cpus not on the critical path of execution is
    reduced thereby lowering energy with little or no adverse impact on
    performance.

    Additional information:

    Bhalachandra, Sridutt, Allan Porterfield, Stephen L. Olivier, and Jan F.
    Prins. "An adaptive core-specific runtime for energy efficiency." In 2017
    IEEE International Parallel and Distributed Processing Symposium (IPDPS),
    pp. 947-956. 2017.

    Note: Power controls (DVFS, DDCM and power capping) needs to be enabled
    before using these interfaces. Please check your architecture specification
    for supported power contols and related information.
"""
import ddcmpolicy
import logging


logger = logging.getLogger('nrm')


class PowerPolicyManager:
    """ Used for power policy application """

    def __init__(self, cpus=None, policy=None, damper=1000000000,
                 slowdown=1.1):
        self.cpus = cpus
        self.policy = policy
        self.damper = damper
        self.slowdown = slowdown

        # Intiliaze all power interfaces
        self.ddcmpolicy = ddcmpolicy.DDCMPolicy()

        # Power levels
        self.maxdclevel = self.ddcmpolicy.maxdclevel
        # TODO: Need to set this value when DVFS policies are added
        self.maxfreqlevel = -1
        self.dclevel = dict.fromkeys(self.cpus, self.maxdclevel)
        self.freqlevel = dict.fromkeys(self.cpus, self.maxfreqlevel)

        # Book-keeping
        self.damperexits = 0
        self.slowdownexits = 0
        self.prevtolalphasetime = dict.fromkeys(self.cpus, None)

    def run_policy(self, phase_contexts):
        # Run only if policy is specified
        if self.policy:
            for id in phase_contexts:
                if id not in self.cpus:
                    logger.info("""Attempt to change power of cpu not in container
                                : %r""", id)
                    return
                # Select and invoke appropriate power policy
                # TODO: Need to add a better policy selection logic in addition
                # to user specified using manifest file
                ret, value = self.execute(id, **phase_contexts[id])
                if self.policy == 'DDCM':
                    if ret == 'DDCM':
                        self.dclevel[id] = value
                    # Incase of slowdown experienced by even process, reset all
                    # cpus
                    if ret == 'SLOWDOWN':
                        self.reset_all()
                phase_contexts[id]['set'] = False

    def execute(self, cpu, **kwargs):
        computetime = kwargs['computetime']
        totalphasetime = kwargs['totaltime']

        # If the current phase length is less than the damper value, then do
        # not use policy. This avoids use of policy during startup operation
        # insignificant phases
        if totalphasetime < self.damper:
            self.damperexits += 1
            return 'DAMPER', -1

        # If the current phase has slowed down beyond the threshold set, then
        # reset power. This helps correct error in policy application or acts
        # as a rudimentary way to detect phase change
        if(self.prevtolalphasetime[cpu] is not None and totalphasetime >
           self.slowdown * self.prevtolalphasetime[cpu]):
            self.ddcmpolicy.dc.reset(cpu)
            newdclevel = self.ddcmpolicy.maxdclevel

            # Reset value for next phase
            self.prevtolalphasetime[cpu] = totalphasetime

            return 'SLOWDOWN', newdclevel

        # Invoke the correct policy based on operation module
        if self.policy == "DDCM":
            newdclevel = self.ddcmpolicy.execute(cpu, self.dclevel[cpu],
                                                 computetime, totalphasetime)
            # Reset value for next phase
            self.prevtolalphasetime[cpu] = totalphasetime

        # TODO: Add DVFS and Combined policies

            return 'DDCM', newdclevel

    def print_policy_stats(self, resetflag=False):
        # Get statistics for policy run
        ppstats = dict()
        ppstats['PowerPolicyDamperExits'] = self.damperexits
        ppstats['PowerPolicySlowdownExits'] = self.slowdownexits
        ppstats.update(self.ddcmpolicy.print_stats(resetflag))
        if resetflag:
            self.damperexits = 0
            self.slowdownexits = 0

        return ppstats

    def power_reset(self, cpu):
        # Reset power control
        self.ddcmpolicy.dc.reset(cpu)

        # Reset value
        self.dclevel[cpu] = self.maxdclevel

    def power_check(self, cpu):
        # Check status of all power controls
        return self.ddcmpolicy.dc.check(cpu)

    def reset_all(self):
        # Reset all cpus
        for cpu in self.cpus:
            self.power_reset(cpu)
