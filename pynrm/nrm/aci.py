###############################################################################
# Copyright 2019 UChicago Argonne, LLC.
# (c.f. AUTHORS, LICENSE)
#
# This file is part of the NRM project.
# For more info, see https://xgitlab.cels.anl.gov/argo/nrm
#
# SPDX-License-Identifier: BSD-3-Clause
###############################################################################

"""Parse and Represent the APPC ACI specification."""
import logging
from schema import loadschema

logger = logging.getLogger('nrm')


def has(self, f):
    return(f in self.app.keys())


ImageManifest = loadschema("yml", "manifest")
setattr(ImageManifest, "is_feature_enabled", has)
