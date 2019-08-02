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
from subprograms import HwlocClient, resources

logger = logging.getLogger('nrm')


class ResourceManager(object):

    """Manages the query of node resources, the tracking of their use and
    the scheduling of new containers according to partitioning rules."""

    def __init__(self, hwloc):
        self.hwloc = HwlocClient(hwloc=hwloc)

        # query the node topo, keep track of the critical resources
        self.allresources = self.hwloc.info()
        logger.debug("resource info: %r", self.allresources)
        self.available = self.allresources
        self.allocations = {}

    def schedule(self, uuid, request):
        """Schedule a resource request on the available resources.

        Request is a dictionary of the resources asked for."""
        # dumb scheduling, just give the first resources available:
        #  - cpus are exclusive
        #  - memories exclusive if more than one left
        if len(self.available.cpus) >= request.cpus:
            retcpus = sorted(self.available.cpus)[:request.cpus]
        else:
            retcpus = []
        if len(self.available.mems) > 1:
            retmems = sorted(self.available.mems)[:request.mems]
        else:
            retmems = self.available.mems
        ret = resources(retcpus, retmems)
        # make sure we don't remember an error
        if ret.cpus:
            self.update(uuid, ret)
        return ret

    def update(self, uuid, allocation=resources([], [])):
        """Update resource tracking according to new allocation.

        The new allocation is saved, and available resources updated."""
        added = {}
        freed = {}
        prev = self.allocations.get(uuid, resources([], []))
        for attr, val in prev._asdict().items():
            added[attr] = set(getattr(allocation, attr)) - set(val)
            freed[attr] = set(val) - set(getattr(allocation, attr))
        if allocation != resources([], []):
            self.allocations[uuid] = allocation
            logger.info("updated allocation for %r: %r", uuid,
                        self.available)
        else:
            del self.allocations[uuid]
            logger.info("deleted allocation for %r", uuid)
        new = {}
        for attr, val in self.available._asdict().items():
            new[attr] = list(set(val) - set(added[attr]) | set(freed[attr]))
        self.available = resources(**new)
        logger.info("updated available resources: %r", self.available)
