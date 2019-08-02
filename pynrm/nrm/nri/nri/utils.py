###############################################################################
# Copyright 2019 UChicago Argonne, LLC.
# (c.f. AUTHORS, LICENSE)
#
# SPDX-License-Identifier: BSD-3-Clause
###############################################################################

"""
Zoo of python control flow and type conversion utilities.
"""

from typing import Callable, Optional, TypeVar, NewType
from nri.types.definitions import _Time, _Return
from nri.types.constructors import mkTime
from nri.types.internal.definitions import Path
import time as t

a = TypeVar("a")
b = TypeVar("b")


# shoving exceptions into an optional.
def tryOpt(f: Callable[[], a]) -> Optional[a]:
    try:
        return f()
    except Exception:
        return None


# applying ('fmapping' although this can have side effects) on an optional.
def fmapOpt(f: Callable[[a], b], ma: Optional[a]) -> Optional[b]:
    if ma is None:
        return None
    else:
        return f(ma)


# applying on an optional with no return code.
def fmapOpt_(f: Callable[[a], None], ma: Optional[a]) -> None:
    if ma is None:
        return
    else:
        f(ma)
        return


# unpacking an opional.
def unpackOpt(opt: Optional[_Return], x: _Return) -> _Return:
    if opt is None:
        return x
    else:
        return opt


# atomic single-line reads on a file
def readStr(fn: Path) -> str:
    with open(fn) as f:
        return f.readline().strip()


def readInt(fn: Path) -> int:
    return int(readStr(fn))


def readBool(fn: Path) -> bool:
    return bool(readStr(fn))


# atomic single-int write on a file
def writeInt(fn: Path, val: int) -> None:
    with open(fn, "w") as f:
        f.write("%d" % val)


# typed time.time()
def time() -> _Time:
    return mkTime(t.time())


# bracket pattern
# def doWith(
# resourceBuilder: Callable[[], a], f: Callable[[a], None]
# ) -> None:
# with resourceBuilder() as resource:
# f(resource)
