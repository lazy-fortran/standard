#!/usr/bin/env python3
"""Utility helpers for loading Fortran fixture source files in tests.

Fixtures live under tests/fixtures/<Standard>/[...] with file extensions:
- .f   : fixed-form Fortran (historical standards)
- .f90 : free-form Fortran (Fortran 90+)
- .lf  : Lazy Fortran source (future)
"""

from pathlib import Path
from typing import Union


_ROOT = Path(__file__).resolve().parent
_FIXTURES_ROOT = _ROOT / "fixtures"


def load_fixture(*relative_parts: Union[str, Path]) -> str:
    """Load a fixture file from tests/fixtures.

    Example:
        code = load_fixture(\"Fortran2003\", \"test_fortran_2003_comprehensive\", \"oop_code.f90\")
    """
    path = _FIXTURES_ROOT
    for part in relative_parts:
        path = path / str(part)
    return path.read_text()

