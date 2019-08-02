"""
Types used in the external nri interface.
"""

from typing import (
    Dict,
    Union,
    NamedTuple,
    Set,
    List,
    NewType,
    TypeVar,
    Optional,
)

_PkgID = NewType("_PkgID", int)
_CoreID = NewType("_CoreID", int)
_PUID = NewType("_PUID", int)
_Temperature = NewType("_Temperature", float)
_MicroJoules = NewType("_MicroJoules", int)
_MicroWatts = NewType("_MicroWatts", int)
_Hz = NewType("_Hz", int)
_Time = NewType("_Time", float)
EnergySamples = NewType("EnergySamples", Dict[_PkgID, _MicroJoules])
_Return = NewType("_Return", bool)
Success = _Return(True)
Failure = _Return(False)


class TemperatureSamples(NamedTuple):
    core_t_celcius: Dict[_CoreID, _Temperature]
    pkg_t_celcius: Dict[_PkgID, _Temperature]


# Observing configuration
class RAPLPackageConfig(NamedTuple):
    enabled: bool
    constraint_0_max_power_uw: _MicroWatts
    constraint_0_name: str
    constraint_0_time_window_us: int
    constraint_1_max_power_uw: _MicroWatts
    constraint_1_name: str
    constraint_1_time_window_us: int


class RAPLConfig(NamedTuple):
    packageConfig: Dict[_PkgID, RAPLPackageConfig]


# control
PcapControl = NewType("PcapControl", Dict[_PkgID, _MicroWatts])
FreqControl = NewType("FreqControl", Dict[_CoreID, _Hz])


# sensing
class MachineInfo(NamedTuple):
    time_last: _Time
    time: _Time
    energySamples: Optional[EnergySamples]
    tempSamples: TemperatureSamples
