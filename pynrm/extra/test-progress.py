#!/usr/bin/env python3

# This snippet tests the progress reporting through the python API.

from nrm import progress
import time
import os

print(os.environ.get("NRM_DOWNSTREAM_EVENT_URI"))
print(os.environ.get("NRM_CMDID"))
print(os.environ.get("NRM_RATELIMIT"))

p = progress.Progress()
p.setup()

for t in range(0, 10):
    time.sleep(1)
    p.progress_report(10)

p.shutdown()
