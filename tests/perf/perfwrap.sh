#!/usr/bin/env bash
 
nrmd ' {verbose =< Error | Info | Debug >.Debug } ' >/dev/null 2>/dev/null  &

nrm run --manifest=../../examples/manifests/perfwrap.dhall -d \
  sleep 10 >/dev/null 2>/dev/null

timeout 15 nrm listen-cpd

pkill -f nrmd 
