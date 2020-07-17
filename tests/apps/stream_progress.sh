#!/usr/bin/env bash

nrmd '{ verbose = < Error | Info | Debug >.Debug,
        raplCfg = None { raplPath : Text,
                         raplActions : List { fromuW : Double },
                         referencePower : { fromuW : Double } }
                       }' >/dev/null 2>/dev/null &

nrm run --manifest=../../hsnrm/resources/example-manifests/libnrm.dhall -d \
  stream_c >/dev/null 2>/dev/null

timeout 10 nrm listen-cpd

pkill stream_c
pkill -f nrmd
