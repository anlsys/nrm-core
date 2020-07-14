#!/usr/bin/env bash

# in case there is a daemon hanging somewhere.
pkill -f nrmd

nrmd '{ verbose = < Error | Info | Debug >.Debug,
        raplCfg =  None { raplPath : Text ,
                          raplActions : List { fromuW : Double } ,
                          referencePower : { fromuW : Double } }}
     ' >/dev/null 2>/dev/null &

# using python nrm library to report progress.
PYTHONPATH=$PYTHONPATH:../../pynrm/ nrm run \
--manifest=../../hsnrm/resources/example-manifests/libnrm.dhall -d \
../../pynrm/extra/test-progress.py >/dev/null 2>/dev/null

# listening to the message on the upstream API
timeout 7 nrm  listen-cpd -j

# cleaning
pkill -f nrmd
