#!/usr/bin/env python3
"""Utility helpers for loading Fortran fixture source files in tests.

Fixtures live under tests/fixtures/<Standard>/[...] with file extensions:
- .f   : fixed-form Fortran (historical standards)
- .f90 : free-form Fortran (Fortran 90+)
- .lf  : Lazy Fortran source (future)
"""

from pathlib import Path
from typing import List, Optional, Type, TypeVar, Union


_ROOT = Path(__file__).resolve().parent
_FIXTURES_ROOT = _ROOT / "fixtures"

TContext = TypeVar("TContext")


def load_fixture(*relative_parts: Union[str, Path]) -> str:
    """Load a fixture file from tests/fixtures.

    Example:
        code = load_fixture(\"Fortran2003\", \"test_fortran_2003_comprehensive\", \"oop_code.f90\")
    """
    path = _FIXTURES_ROOT
    for part in relative_parts:
        path = path / str(part)
    return path.read_text()


def find_contexts(node, ctx_type: Type[TContext]) -> List[TContext]:
    """Collect all parse-tree contexts of a given type.

    This is a lightweight ANTLR4 parse-tree walk used by tests.
    """
    contexts: List[TContext] = []
    if node is None:
        return contexts

    stack = [node]
    while stack:
        current = stack.pop()
        if isinstance(current, ctx_type):
            contexts.append(current)
        if hasattr(current, "getChildCount"):
            for i in range(current.getChildCount()):
                stack.append(current.getChild(i))

    return contexts
