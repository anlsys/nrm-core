# (generated with --quick)

import nri.types.definitions
import nri.types.internal.definitions
from typing import Type, TypeVar

Failure: nri.types.definitions._Return
FreqControl: Type[nri.types.definitions.FreqControl]
Path: Type[nri.types.internal.definitions.Path]
_CoreID: Type[nri.types.definitions._CoreID]
_MicroWatts: Type[nri.types.definitions._MicroWatts]
_Return: Type[nri.types.definitions._Return]
_msr_powerlimit_location: int
_msr_unit_location: int
errno: module
math: module
os: module
struct: module
sys: module

_T2 = TypeVar('_T2')

class Msr:
    def _insertbits(self, val: int, l: int, h: int, newval: float) -> int: ...
    def file_open(self, filename: nri.types.internal.definitions.Path, privilege: str) -> int: ...
    def get_file_path(self, cpu: nri.types.definitions._CoreID) -> nri.types.internal.definitions.Path: ...
    def read(self, cpu: nri.types.definitions._CoreID, register: int) -> int: ...
    def set_powerlimit(self, cpu: nri.types.definitions._CoreID, watts: nri.types.definitions._MicroWatts) -> None: ...
    def setfrequencies(self, control: nri.types.definitions.FreqControl) -> nri.types.definitions._Return: ...
    def write(self, cpu: nri.types.definitions._CoreID, register: int, value: _T2) -> _T2: ...
