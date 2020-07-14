#!/usr/bin/env bash
nrmd 2>/dev/null >/dev/null &
nrm run -- bash -c 'exit 3'
EC=$?
pkill -f nrmd 2>/dev/null >/dev/null
exit ${EC}
