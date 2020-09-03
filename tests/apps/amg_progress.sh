#!/usr/bin/env bash

nrmd '{ verbose = < Error | Info | Debug >.Debug,
        raplCfg = None { raplPath : Text,
                         raplActions : List { fromuW : Double },
                         referencePower : { fromuW : Double } }
                       }'  &

OMP_NUM_THREADS=1 nrm run --manifest=../../hsnrm/resources/example-manifests/libnrm.dhall -d \
  -- mpiexec -n 2 amg -problem 2 -n 90 90 90 -P 2 1 1

timeout 10 nrm listen-cpd

pkill amg
pkill -f nrmd
