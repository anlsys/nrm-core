#!/usr/bin/env bash

nrmd ' let t = ../../hsnrm/hsnrm/dhall/types/nrmd.dhall
       let d = ../../hsnrm/hsnrm/dhall/defaults/nrmd.dhall
       in d // { verbose = t.Verbosity.Debug,
                 raplCfg =  None t.RaplCfg
               }
     ' &

OMP_NUM_THREADS=1 nrm run --manifest=../../examples/manifests/libnrm.dhall -d \
  -- mpiexec -n 2 amg -problem 2 -n 90 90 90 -P 2 1 1

timeout 10 nrm listen-cpd

pkill amg
pkill -f nrmd
