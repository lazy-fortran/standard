#!/usr/bin/env python3
"""Generic fixture-based parsing tests for standard Fortran grammars.

This module auto-discovers Fortran source fixtures under tests/fixtures/<Standard>/
and verifies that each example parses with zero syntax errors using the
appropriate lexer/parser and top-level program_unit rule for that standard.

The goal is that adding a new .f / .f90 / .lf fixture file automatically
adds coverage, without needing to edit per-standard pytest files.
"""

from __future__ import annotations

import sys
from pathlib import Path
from typing import Dict, List, Tuple

import pytest

# Make grammars and fixture utilities importable
ROOT = Path(__file__).resolve().parent
GRAMMARS = ROOT.parent / "grammars"
sys.path.insert(0, str(GRAMMARS / "generated" / "early"))
sys.path.insert(0, str(GRAMMARS / "generated" / "modern"))
sys.path.append(str(ROOT))

from fixture_utils import load_fixture  # noqa: E402
from xpass_fixtures import XPASS_FIXTURES  # noqa: E402

FIXTURES_ROOT = ROOT / "fixtures"


class StandardConfig:
    """Configuration for a standard's lexer/parser and entry rule."""

    def __init__(self, lexer_name: str, parser_name: str, entry_rule: str):
        self.lexer_name = lexer_name
        self.parser_name = parser_name
        self.entry_rule = entry_rule
        self.lexer_cls = None
        self.parser_cls = None
        self._import()

    def _import(self) -> None:
        try:
            module = __import__(self.lexer_name, fromlist=[self.lexer_name])
            self.lexer_cls = getattr(module, self.lexer_name)
            parser_module = __import__(self.parser_name, fromlist=[self.parser_name])
            self.parser_cls = getattr(parser_module, self.parser_name)
        except Exception:
            # Leave classes as None; tests will be skipped for this standard.
            self.lexer_cls = None
            self.parser_cls = None

    @property
    def available(self) -> bool:
        return self.lexer_cls is not None and self.parser_cls is not None


STANDARD_CONFIGS: Dict[str, StandardConfig] = {
    # Historical FORTRAN grammars – included in dynamic discovery so that
    # their fixtures are exercised automatically. Known rough edges are
    # explicitly marked in XPASS_FIXTURES.
    "FORTRAN": StandardConfig(
        lexer_name="FORTRANLexer",
        parser_name="FORTRANParser",
        entry_rule="program_unit_core",
    ),
    "FORTRANII": StandardConfig(
        lexer_name="FORTRANIILexer",
        parser_name="FORTRANIIParser",
        entry_rule="fortran_program",
    ),
    "FORTRAN66": StandardConfig(
        lexer_name="FORTRAN66Lexer",
        parser_name="FORTRAN66Parser",
        entry_rule="fortran66_program",
    ),
    "FORTRAN77": StandardConfig(
        lexer_name="FORTRAN77Lexer",
        parser_name="FORTRAN77Parser",
        entry_rule="main_program",
    ),
    # Modern standardized Fortran grammars.
    "Fortran90": StandardConfig(
        lexer_name="Fortran90Lexer",
        parser_name="Fortran90Parser",
        entry_rule="program_unit_f90",
    ),
    "Fortran95": StandardConfig(
        lexer_name="Fortran95Lexer",
        parser_name="Fortran95Parser",
        entry_rule="program_unit_f95",
    ),
    "Fortran2003": StandardConfig(
        lexer_name="Fortran2003Lexer",
        parser_name="Fortran2003Parser",
        entry_rule="program_unit_f2003",
    ),
    "Fortran2008": StandardConfig(
        lexer_name="Fortran2008Lexer",
        parser_name="Fortran2008Parser",
        entry_rule="program_unit_f2008",
    ),
    "Fortran2023": StandardConfig(
        lexer_name="Fortran2023Lexer",
        parser_name="Fortran2023Parser",
        entry_rule="program_unit_f2023",
    ),
}


def _discover_fixtures() -> List[Tuple[str, Path]]:
    """Discover all .f / .f90 / .lf fixtures under tests/fixtures."""
    cases: List[Tuple[str, Path]] = []
    if not FIXTURES_ROOT.exists():
        return cases

    for standard_dir in FIXTURES_ROOT.iterdir():
        if not standard_dir.is_dir():
            continue
        standard = standard_dir.name
        if standard not in STANDARD_CONFIGS:
            continue
        for path in standard_dir.rglob("*"):
            if not path.is_file():
                continue
            if path.suffix.lower() not in {".f", ".f90", ".lf"}:
                continue
            rel = path.relative_to(FIXTURES_ROOT)
            cases.append((standard, rel))

    # Deterministic order for stable test ids
    cases.sort(key=lambda item: str(item[1]))
    return cases


FIXTURE_CASES = _discover_fixtures()


@pytest.mark.parametrize("standard, relpath", FIXTURE_CASES)
def test_fixture_parses_without_errors(standard: str, relpath: Path) -> None:
    """Ensure each fixture parses with zero syntax errors for its standard."""
    cfg = STANDARD_CONFIGS[standard]
    if not cfg.available:
        pytest.skip(f"{standard} lexer/parser not available")

    # Lazy import to avoid importing antlr4 at module import time if unnecessary
    from antlr4 import InputStream, CommonTokenStream  # type: ignore

    # Load code from fixture and parse
    code = load_fixture(*relpath.parts)
    input_stream = InputStream(code)
    lexer = cfg.lexer_cls(input_stream)
    parser = cfg.parser_cls(CommonTokenStream(lexer))

    entry = getattr(parser, cfg.entry_rule)
    tree = entry()
    errors = parser.getNumberOfSyntaxErrors()

    message = (
        f"{standard} fixture {relpath} did not produce a parse tree"
    )
    assert tree is not None, message

    key = (standard, relpath)
    if key in XPASS_FIXTURES:
        reason_template = XPASS_FIXTURES[key]
        pytest.xfail(
            reason_template.format(
                standard=standard,
                relpath=relpath,
                errors=errors,
            )
        )

    assert errors == 0, (
        f"{standard} fixture {relpath} expected 0 syntax errors, got {errors}"
    )
