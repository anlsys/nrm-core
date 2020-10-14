#!/usr/bin/env bash

# in case there is a daemon hanging somewhere.
pkill -f nrmd

nrmd ' let t = ../../hsnrm/hsnrm/dhall/types/nrmd.dhall
       let d = ../../hsnrm/hsnrm/dhall/defaults/nrmd.dhall
       in d // { verbose = t.Verbosity.Debug,
                 raplCfg =  None t.RaplCfg
               }
     ' >/dev/null 2>/dev/null &

# using python nrm library to report progress.
PYTHONPATH=$PYTHONPATH:../../pynrm/ nrm run \
--manifest=../../examples/manifests/libnrm.dhall -d \
../../pynrm/extra/test-progress.py >/dev/null 2>/dev/null

# listening to the message on the upstream API
timeout 7 nrm  listen-cpd -j

# cleaning
pkill -f nrmd
