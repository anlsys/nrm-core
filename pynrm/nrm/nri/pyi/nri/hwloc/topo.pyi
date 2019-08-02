# (generated with --quick)

import nri.types.definitions
from typing import Callable, List, SupportsInt, Type, TypeVar, Union
import xml.etree.ElementTree

_CoreID: Type[nri.types.definitions._CoreID]
_PUID: Type[nri.types.definitions._PUID]
_PkgID: Type[nri.types.definitions._PkgID]
subprocess: module
xml: module

_T = TypeVar('_T')

class Topology:
    __doc__: str
    tree: xml.etree.ElementTree.Element
    def __init__(self, cmd: str = ...) -> None: ...
    def list_core_ids(self) -> List[nri.types.definitions._CoreID]: ...
    def list_processing_unit_ids(self) -> List[nri.types.definitions._PUID]: ...

@overload
def dataclass(_cls: Type[_T]) -> Type[_T]: ...
@overload
def dataclass(*, init: bool = ..., repr: bool = ..., eq: bool = ..., order: bool = ..., unsafe_hash: bool = ..., frozen: bool = ...) -> Callable[[Type[_T]], Type[_T]]: ...
def mkCoreID(s: Union[str, SupportsInt]) -> nri.types.definitions._CoreID: ...
def mkPUID(s: Union[str, SupportsInt]) -> nri.types.definitions._PUID: ...
def mkPkgID(s: Union[str, SupportsInt]) -> nri.types.definitions._PkgID: ...
