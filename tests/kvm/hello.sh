#!/usr/bin/env bash
nrmd &
nrm run echo foo
pkill -f nrmd
