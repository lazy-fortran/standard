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


def load_fixture(*relative_parts: Union[str, Path]) -> str:
    """Load a fixture file from tests/fixtures.

    Example:
        code = load_fixture(\"Fortran2003\", \"test_fortran_2003_comprehensive\", \"oop_code.f90\")
    """
    path = _FIXTURES_ROOT
    for part in relative_parts:
        path = path / str(part)
    return path.read_text()


TContext = TypeVar("TContext")


def find_contexts(node, ctx_type: Type[TContext]) -> List[TContext]:
    """Recursively collect all ANTLR contexts of a given type under node."""
    contexts: List[TContext] = []
    if node is None:
        return contexts
    if isinstance(node, ctx_type):
        contexts.append(node)
    if hasattr(node, "getChildCount"):
        for i in range(node.getChildCount()):
            contexts.extend(find_contexts(node.getChild(i), ctx_type))
    return contexts


def find_first_context(node, ctx_type: Type[TContext]) -> Optional[TContext]:
    """Return the first matching context of a given type, if any."""
    matches = find_contexts(node, ctx_type)
    return matches[0] if matches else None
