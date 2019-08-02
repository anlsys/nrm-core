from typing import Callable, Optional, TypeVar, NewType, SupportsInt, Union
from nri.types.definitions import (
    _PkgID,
    _CoreID,
    _PUID,
    _Temperature,
    _MicroJoules,
    _MicroWatts,
    _Hz,
    _Time,
)

"""
Smart constructors for nri types.
"""

a = TypeVar("a")


def mkPkgID(s: Union[str, SupportsInt]) -> _PkgID:
    i = int(s)
    if i >= 0:
        return _PkgID(i)
    else:
        raise (ValueError("Negative Package ID! Can't build PkgID datatype."))


def mkCoreID(s: Union[str, SupportsInt]) -> _CoreID:
    i = int(s)
    if i >= 0:
        return _CoreID(i)
    else:
        raise (ValueError("Negative Core ID! Can't build CoreID datatype."))


def mkPUID(s: Union[str, SupportsInt]) -> _PUID:
    i = int(s)
    if i >= 0:
        return _PUID(i)
    else:
        raise (
            ValueError(
                "Negative processing unit ID! Can't build PUID datatype."
            )
        )


def mkTemperature(s: Union[str, SupportsInt]) -> _Temperature:
    i = int(s)
    if i >= 0:
        return _Temperature(i)
    else:
        raise (
            ValueError(
                "Negative temperature! Can't build Temperature datatype."
            )
        )


def mkMicroJoules(s: Union[str, SupportsInt]) -> _MicroJoules:
    i = int(s)
    if i >= 0:
        return _MicroJoules(i)
    else:
        raise (
            ValueError("Negative energy! Can't build MicroJoules datatype.")
        )


def mkMicroWatts(s: Union[str, SupportsInt]) -> _MicroWatts:
    i = int(s)
    if i >= 0:
        return _MicroWatts(i)
    else:
        raise (ValueError("Negative power! Can't build MicroWatts datatype."))


def mkHz(s: Union[str, SupportsInt]) -> _Hz:
    i = int(s)
    if i >= 0:
        return _Hz(i)
    else:
        raise (ValueError("Negative frequency! Can't build Hz datatype."))


def mkTime(s: Union[str, SupportsInt]) -> _Time:
    i = int(s)
    if i >= 0:
        return _Time(i)
    else:
        raise (ValueError("Negative timestamp! Can't build Time datatype."))
