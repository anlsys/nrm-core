# (generated with --quick)

import collections
import nri.types.definitions
import nri.types.internal.definitions
from typing import Callable, Dict, Iterable, Pattern, Sized, SupportsInt, Tuple, Type, TypeVar, Union

EnergySamples: Type[nri.types.definitions.EnergySamples]
Failure: nri.types.definitions._Return
Path: Type[nri.types.internal.definitions.Path]
PcapControl: Type[nri.types.definitions.PcapControl]
RAPLConfig: Type[nri.types.definitions.RAPLConfig]
RAPLPackageConfig: Type[nri.types.definitions.RAPLPackageConfig]
Success: nri.types.definitions._Return
_MicroJoules: Type[nri.types.definitions._MicroJoules]
_MicroWatts: Type[nri.types.definitions._MicroWatts]
_PkgID: Type[nri.types.definitions._PkgID]
_Return: Type[nri.types.definitions._Return]
getopt: module
os: module
re: module
sys: module
time: module

_TEnergyRead = TypeVar('_TEnergyRead', bound=EnergyRead)

class EnergyRead(tuple):
    __slots__ = ["reads", "time"]
    __dict__: collections.OrderedDict[str, Union[float, nri.types.definitions.EnergySamples]]
    _field_defaults: collections.OrderedDict[str, Union[float, nri.types.definitions.EnergySamples]]
    _field_types: collections.OrderedDict[str, type]
    _fields: Tuple[str, str]
    reads: nri.types.definitions.EnergySamples
    time: float
    def __getnewargs__(self) -> Tuple[float, nri.types.definitions.EnergySamples]: ...
    def __getstate__(self) -> None: ...
    def __init__(self, *args, **kwargs) -> None: ...
    def __new__(cls: Type[_TEnergyRead], time: float, reads: nri.types.definitions.EnergySamples) -> _TEnergyRead: ...
    def _asdict(self) -> collections.OrderedDict[str, Union[float, nri.types.definitions.EnergySamples]]: ...
    @classmethod
    def _make(cls: Type[_TEnergyRead], iterable: Iterable[Union[float, nri.types.definitions.EnergySamples]], new = ..., len: Callable[[Sized], int] = ...) -> _TEnergyRead: ...
    def _replace(self: _TEnergyRead, **kwds: Union[float, nri.types.definitions.EnergySamples]) -> _TEnergyRead: ...

class Rapl:
    dirs: Dict[nri.types.definitions._PkgID, nri.types.internal.definitions.Path]
    initialized: bool
    lastpower: dict
    max_energy_range_uj_d: Dict[nri.types.definitions._PkgID, nri.types.definitions._MicroJoules]
    prev_e: EnergyRead
    rapldir: nri.types.internal.definitions.Path
    re_domain: Pattern[str]
    totalenergy: dict
    def __init__(self) -> None: ...
    def diffenergy(self, e1: EnergyRead, e2: EnergyRead) -> EnergyRead: ...
    def read_config(self) -> nri.types.definitions.RAPLConfig: ...
    def readenergy(self) -> EnergyRead: ...
    def sample(self) -> nri.types.definitions.EnergySamples: ...
    def setpcaps(self, pcaps: nri.types.definitions.PcapControl) -> nri.types.definitions._Return: ...

def mkMicroJoules(s: Union[str, SupportsInt]) -> nri.types.definitions._MicroJoules: ...
def mkMicroWatts(s: Union[str, SupportsInt]) -> nri.types.definitions._MicroWatts: ...
def readBool(fn: nri.types.internal.definitions.Path) -> bool: ...
def readInt(fn: nri.types.internal.definitions.Path) -> int: ...
def readStr(fn: nri.types.internal.definitions.Path) -> str: ...
def writeInt(fn: nri.types.internal.definitions.Path, val: int) -> None: ...
