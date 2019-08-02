# (generated with --quick)

import nri.hwloc.topo
import nri.msr.msr
import nri.sysfs.nodeinfo
import nri.sysfs.rapl
import nri.sysfs.temp
import nri.types.definitions
from typing import Any, Callable, Mapping, Optional, Type, TypeVar

Coretemp: Type[nri.sysfs.temp.Coretemp]
Failure: nri.types.definitions._Return
FreqControl: Type[nri.types.definitions.FreqControl]
MachineInfo: Type[nri.types.definitions.MachineInfo]
Msr: Type[nri.msr.msr.Msr]
Nodeconfig: Type[nri.sysfs.nodeinfo.Nodeconfig]
PcapControl: Type[nri.types.definitions.PcapControl]
RAPLConfig: Type[nri.types.definitions.RAPLConfig]
Rapl: Type[nri.sysfs.rapl.Rapl]
Topology: Type[nri.hwloc.topo.Topology]
_Return: Type[nri.types.definitions._Return]
_Time: Type[nri.types.definitions._Time]

_T = TypeVar('_T')
_Tnode = TypeVar('_Tnode', bound=node)
a = TypeVar('a')
b = TypeVar('b')

class node:
    _coretemp: nri.sysfs.temp.Coretemp
    _msr: Optional[nri.msr.msr.Msr]
    _nodeconfig: nri.sysfs.nodeinfo.Nodeconfig
    _rapl: Optional[nri.sysfs.rapl.Rapl]
    _time_last: nri.types.definitions._Time
    nodename: str
    topology: nri.hwloc.topo.Topology
    def __enter__(self: _Tnode) -> _Tnode: ...
    def __exit__(self, type: Exception, value: Exception, traceback: str) -> None: ...
    def do_control_freq(self, control: nri.types.definitions.FreqControl) -> nri.types.definitions._Return: ...
    def do_control_pcap(self, control: nri.types.definitions.PcapControl) -> nri.types.definitions._Return: ...
    def read_rapl_config(self) -> Optional[nri.types.definitions.RAPLConfig]: ...
    def sample(self) -> nri.types.definitions.MachineInfo: ...

@overload
def dataclass(_cls: Type[_T]) -> Type[_T]: ...
@overload
def dataclass(*, init: bool = ..., repr: bool = ..., eq: bool = ..., order: bool = ..., unsafe_hash: bool = ..., frozen: bool = ...) -> Callable[[Type[_T]], Type[_T]]: ...
@overload
def field(*, default: _T, init: bool = ..., repr: bool = ..., hash: bool = ..., compare: bool = ..., metadata: Optional[Mapping[str, Any]] = ...) -> _T: ...
@overload
def field(*, default_factory: Callable[[], _T], init: bool = ..., repr: bool = ..., hash: bool = ..., compare: bool = ..., metadata: Optional[Mapping[str, Any]] = ...) -> _T: ...
@overload
def field(*, init: bool = ..., repr: bool = ..., hash: bool = ..., compare: bool = ..., metadata: Optional[Mapping[str, Any]] = ...) -> Any: ...
def fmapOpt(f: Callable[[a], b], ma: Optional[a]) -> Optional[b]: ...
def fmapOpt_(f: Callable[[a], None], ma: Optional[a]) -> None: ...
def time() -> nri.types.definitions._Time: ...
def tryOpt(f: Callable[[], a]) -> Optional[a]: ...
def unpackOpt(opt: Optional[nri.types.definitions._Return], x: nri.types.definitions._Return) -> nri.types.definitions._Return: ...
