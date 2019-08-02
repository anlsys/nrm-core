# (generated with --quick)

import nri.msr.msr
import nri.types.definitions
from typing import Type

_CoreID: Type[nri.types.definitions._CoreID]
nri: module

class DutyCycle:
    msr: nri.msr.msr.Msr
    register: int
    def __init__(self) -> None: ...
    def check(self, cpu: nri.types.definitions._CoreID) -> int: ...
    def reset(self, cpu: nri.types.definitions._CoreID) -> None: ...
    def set(self, cpu: nri.types.definitions._CoreID, value: int) -> None: ...
