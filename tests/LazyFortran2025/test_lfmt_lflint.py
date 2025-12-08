#!/usr/bin/env python3
"""Tests for Lazy Fortran formatter and linter."""

from __future__ import annotations

import os
import subprocess
import sys
from pathlib import Path

import pytest


PROJECT_ROOT = Path(__file__).resolve().parents[2]
GRAMMARS_DIR = PROJECT_ROOT / "grammars/generated/modern"

sys.path.insert(0, str(PROJECT_ROOT))
sys.path.insert(0, str(GRAMMARS_DIR))


try:
    from LazyFortran2025Lexer import LazyFortran2025Lexer  # type: ignore
    from LazyFortran2025Parser import LazyFortran2025Parser  # type: ignore

    PARSER_AVAILABLE = True
except ImportError as exc:  # pragma: no cover - environment dependent
    PARSER_AVAILABLE = False
    LazyFortran2025Lexer = None  # type: ignore[assignment]
    LazyFortran2025Parser = None  # type: ignore[assignment]
    pytest.skip(f"LazyFortran2025 grammar not built: {exc}", allow_module_level=True)


from lazyfortran_tooling import format_lazy_code, lint_lazy_code  # noqa: E402


@pytest.mark.skipif(not PARSER_AVAILABLE, reason="LazyFortran2025 parser not available")
def test_format_inline_if_to_block_is_idempotent() -> None:
    """lfmt transforms compact IF into canonical block form."""
    source = 'if(x>0)print*,"ok"\n'
    expected = (
        "if (x > 0) then\n"
        '  print *, "ok"\n'
        "end if\n"
    )

    formatted = format_lazy_code(source)
    assert formatted == expected
    assert format_lazy_code(formatted) == expected


@pytest.mark.skipif(not PARSER_AVAILABLE, reason="LazyFortran2025 parser not available")
def test_format_inline_if_with_relational_operator() -> None:
    """lfmt handles >= inside inline IF conditions."""
    source = 'if(x>=0)print*,"ok"\n'
    expected = (
        "if (x >= 0) then\n"
        '  print *, "ok"\n'
        "end if\n"
    )

    formatted = format_lazy_code(source)
    assert formatted == expected
    assert format_lazy_code(formatted) == expected


@pytest.mark.skipif(not PARSER_AVAILABLE, reason="LazyFortran2025 parser not available")
def test_lint_reports_missing_implicit_none() -> None:
    """lflint reports missing IMPLICIT NONE."""
    code = (
        "x = 1\n"
        "print *, x\n"
    )

    issues = lint_lazy_code(code)
    codes = {issue.code for issue in issues}
    assert "LF001" in codes


@pytest.mark.skipif(not PARSER_AVAILABLE, reason="LazyFortran2025 parser not available")
def test_lint_respects_implicit_none() -> None:
    """lflint does not warn when IMPLICIT NONE is present."""
    code = (
        "implicit none\n"
        "integer :: x\n"
        "x = 1\n"
        "print *, x\n"
    )

    issues = lint_lazy_code(code)
    codes = {issue.code for issue in issues}
    assert "LF001" not in codes


@pytest.mark.skipif(not PARSER_AVAILABLE, reason="LazyFortran2025 parser not available")
def test_lfmt_cli_formats_file_in_place(tmp_path: Path) -> None:
    """lfmt CLI formats a file on disk."""
    source = 'if(x>0)print*,"ok"\n'
    expected = (
        "if (x > 0) then\n"
        '  print *, "ok"\n'
        "end if\n"
    )

    path = tmp_path / "main.lf"
    path.write_text(source)

    cmd = [sys.executable, str(PROJECT_ROOT / "lfmt"), str(path)]
    result = subprocess.run(
        cmd,
        cwd=str(PROJECT_ROOT),
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        check=False,
    )

    assert result.returncode == 0, result.stderr
    assert path.read_text() == expected


@pytest.mark.skipif(not PARSER_AVAILABLE, reason="LazyFortran2025 parser not available")
def test_lflint_cli_reports_and_sets_exit_code(tmp_path: Path) -> None:
    """lflint CLI reports issues and returns non-zero exit code."""
    code = (
        "x = 1\n"
        "print *, x\n"
    )

    path = tmp_path / "prog.lf"
    path.write_text(code)

    cmd = [sys.executable, str(PROJECT_ROOT / "lflint"), str(path)]
    result = subprocess.run(
        cmd,
        cwd=str(PROJECT_ROOT),
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        check=False,
    )

    assert result.returncode == 1
    assert "LF001" in result.stdout
    assert str(path) in result.stdout
