###############################################################################
# Copyright 2019 UChicago Argonne, LLC.
# (c.f. AUTHORS, LICENSE)
#
# This file is part of the NRM project.
# For more info, see https://xgitlab.cels.anl.gov/argo/nrm
#
# SPDX-License-Identifier: BSD-3-Clause
###############################################################################

import warlock # type: ignore
import json
import yaml
import os
from jsonschema import Draft4Validator # type: ignore

_jsonexts = ["json"]
_yamlexts = ["yml", "yaml"]


def loadschema(ext: str, api: str):
    sourcedir = os.path.dirname(os.path.realpath(__file__))
    with open(os.path.join(sourcedir, "schemas", api+"."+ext)) as f:
        if ext in _jsonexts:
            s = json.load(f)
        elif ext in _yamlexts:
            s = yaml.load(f)
        else:
            raise("Schema extension not in %s" % str(_jsonexts + _yamlexts))
        Draft4Validator.check_schema(s)
        return(warlock.model_factory(s))
