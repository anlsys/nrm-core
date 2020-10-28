#!/usr/bin/env bash

nrmd ' let t = ../../hsnrm/hsnrm/dhall/types/nrmd.dhall
       let d = ../../hsnrm/hsnrm/dhall/defaults/nrmd.dhall
       in d // { verbose = t.Verbosity.Debug,
                 raplCfg =  None t.RaplCfg
               }
     ' >/dev/null 2>/dev/null &

nrm run --manifest=../../examples/manifests/libnrm.dhall -d \
  stream_c >/dev/null 2>/dev/null

timeout 10 nrm listen-cpd

pkill stream_c
pkill -f nrmd
