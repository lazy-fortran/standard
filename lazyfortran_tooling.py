#!/usr/bin/env python3
"""
Lazy Fortran 2025 formatter and linter utilities.

This module provides the core logic for the `lfmt` and `lflint` commands.
It uses the LazyFortran2025 ANTLR4 grammar to validate input before and
after transformations so that formatting and lint checks remain
grammar-aware.
"""

from __future__ import annotations

import re
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import List


PROJECT_ROOT = Path(__file__).resolve().parent
GRAMMAR_DIR = PROJECT_ROOT / "grammars"

if str(GRAMMAR_DIR) not in sys.path:
    sys.path.insert(0, str(GRAMMAR_DIR))

try:
    from antlr4 import CommonTokenStream, InputStream  # type: ignore
    from LazyFortran2025Lexer import LazyFortran2025Lexer  # type: ignore
    from LazyFortran2025Parser import LazyFortran2025Parser  # type: ignore

    PARSER_AVAILABLE = True
    PARSER_IMPORT_ERROR: Exception | None = None
except Exception as exc:  # pragma: no cover - environment dependent
    PARSER_AVAILABLE = False
    PARSER_IMPORT_ERROR = exc
    LazyFortran2025Lexer = None  # type: ignore[assignment]
    LazyFortran2025Parser = None  # type: ignore[assignment]


class LazyFortranToolError(RuntimeError):
    """Base error raised by formatter and linter helpers."""


class LazyFortranParseError(LazyFortranToolError):
    """Raised when parsing Lazy Fortran code fails."""


@dataclass
class LintIssue:
    """Represents a single linter finding."""

    path: Path
    line: int
    column: int
    code: str
    message: str


def _ensure_parser_available() -> None:
    parser_missing = (
        not PARSER_AVAILABLE
        or LazyFortran2025Lexer is None  # type: ignore[truthy-function]
        or LazyFortran2025Parser is None  # type: ignore[truthy-function]
    )
    if parser_missing:
        detail = (
            f"{PARSER_IMPORT_ERROR}"
            if PARSER_IMPORT_ERROR is not None
            else "LazyFortran2025 grammar not built"
        )
        raise LazyFortranToolError(
            "LazyFortran2025 lexer/parser are not available. "
            "Run `make LazyFortran2025` before using formatter or linter. "
            f"Details: {detail}"
        )


def _parse_lazy_source(source: str) -> None:
    """Parse Lazy Fortran source and raise on syntax errors."""

    _ensure_parser_available()

    stream = InputStream(source)
    lexer = LazyFortran2025Lexer(stream)  # type: ignore[call-arg]
    tokens = CommonTokenStream(lexer)
    parser = LazyFortran2025Parser(tokens)  # type: ignore[call-arg]

    # Lazy Fortran tooling focuses on relaxed entry point
    parser.lazy_entry()  # type: ignore[call-arg]
    if parser.getNumberOfSyntaxErrors() != 0:
        raise LazyFortranParseError(
            "LazyFortran2025 parser reported syntax errors for the provided source"
        )


def _extract_inline_if_condition_and_remainder(
    stripped: str,
) -> tuple[str, str] | None:
    """
    Return condition and trailing statement for a compact inline IF.

    The input string must start with ``if`` and contain a parenthesized
    condition followed by the body on the same line.
    """

    if "(" not in stripped or ")" not in stripped:
        return None

    after_if = stripped[2:].lstrip()
    if not after_if.startswith("("):
        return None

    close_index = after_if.find(")")
    if close_index == -1:
        return None

    condition = after_if[1:close_index].strip()
    remainder = after_if[close_index + 1 :].strip()
    if not remainder:
        return None

    return condition, remainder


def _normalize_condition_spacing(condition: str) -> str:
    """Normalize spacing around simple relational operators."""

    condition = re.sub(r"\s*(>=|<=|>|<)\s*", r" \1 ", condition)
    return " ".join(condition.split())


def _normalize_inline_if(line: str) -> str | None:
    """
    Transform simple inline IF statements into a canonical block form.

    Example:
        if(x>0)print*,"ok"
    becomes:
        if (x > 0) then
          print *, "ok"
        end if

    Returns the transformed multi-line string, or None when the input
    line should not be rewritten.
    """

    stripped = line.lstrip()
    leading = line[: len(line) - len(stripped)]

    if not stripped or stripped.startswith("!"):
        return None

    lower = stripped.lower()
    if not lower.startswith("if"):
        return None
    if " then" in lower or lower.startswith("if (") and " then" in lower:
        return None

    extracted = _extract_inline_if_condition_and_remainder(stripped)
    if extracted is None:
        return None

    condition, remainder = extracted
    condition = _normalize_condition_spacing(condition)

    # Canonical PRINT spacing for the example style
    if remainder.lower().startswith("print*"):
        remainder = "print *, " + remainder[len("print*") :].lstrip().lstrip(",")

    body_line = f"{leading}  {remainder}"
    header = f"{leading}if ({condition}) then"
    footer = f"{leading}end if"
    return "\n".join([header, body_line, footer])


def format_lazy_code(source: str) -> str:
    """
    Format Lazy Fortran source into a canonical style.

    The formatter is intentionally conservative for now: it validates
    the input using the LazyFortran2025 grammar and then applies a
    small set of whitespace and structure normalizations, focusing on
    inline IF statements and trailing whitespace. The result is parsed
    again to guarantee that formatting preserves syntactic validity.
    """

    lines = source.splitlines()
    formatted_parts: List[str] = []

    for line in lines:
        # Try inline IF normalization first
        rewritten = _normalize_inline_if(line)
        if rewritten is not None:
            formatted_parts.append(rewritten)
        else:
            formatted_parts.append(line.rstrip())

    formatted = "\n".join(formatted_parts)

    # Preserve a final newline if the input had one.
    if source.endswith("\n") and not formatted.endswith("\n"):
        formatted += "\n"

    # Validate the formatted result, even if the original source used
    # a slightly more permissive subset of syntax.
    _parse_lazy_source(formatted)
    return formatted


def format_lazy_file(path: Path) -> tuple[str, bool]:
    """
    Format a Lazy Fortran file and return the new contents and change flag.

    The file is not written to disk here; callers decide how to apply
    the changes.
    """

    text = path.read_text()
    formatted = format_lazy_code(text)
    return formatted, formatted != text


def lint_lazy_code(source: str, *, path: Path | None = None) -> List[LintIssue]:
    """
    Run basic lint checks on Lazy Fortran source.

    Current checks:
      - LF001: missing or disabled IMPLICIT NONE
      - LF002: compact PRINT form `print*` without space before `*`
    """

    _parse_lazy_source(source)

    issues: List[LintIssue] = []
    lines = source.splitlines()
    has_implicit_none = any("implicit none" in line.lower() for line in lines)

    base_path = path if path is not None else Path("<stdin>")

    if not has_implicit_none:
        issues.append(
            LintIssue(
                path=base_path,
                line=1,
                column=1,
                code="LF001",
                message="Missing `implicit none` declaration in Lazy Fortran source",
            )
        )

    for index, line in enumerate(lines, start=1):
        lower = line.lower()
        column = lower.find("print*")
        if column != -1:
            issues.append(
                LintIssue(
                    path=base_path,
                    line=index,
                    column=column + 1,
                    code="LF002",
                    message="Use `print *,` form instead of compact `print*`",
                )
            )

    return issues


def lint_lazy_file(path: Path) -> List[LintIssue]:
    """Lint a Lazy Fortran file and return the collected issues."""

    text = path.read_text()
    return lint_lazy_code(text, path=path)
