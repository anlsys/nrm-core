#!/usr/bin/env bash
 
nrmd 2>/dev/null >/dev/null &

timeout 5 nrm listen-cpd

pkill -f nrmd
