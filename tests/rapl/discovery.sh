#!/usr/bin/env bash
nrmd 2>/dev/null >/dev/null &
nrm cpd
pkill -f nrmd
exit ${EC}
