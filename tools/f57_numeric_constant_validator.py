#!/usr/bin/env python3
"""
Semantic validation for IBM 704 FORTRAN (1957) numeric constants.

Reference: IBM FORTRAN Automatic Coding System for the IBM 704
Data Processing System (Form C28-6003, Oct 1958), Chapter II (Constants).

Expected behavior (per the manual; see issue #667):
- Fixed point constants are 1–5 decimal digits and their magnitude must be
  less than 32768.
- Floating point constants must be zero or lie within the approximate limits
  10^-38 to 10^38.
"""

from __future__ import annotations

from dataclasses import dataclass
from decimal import Decimal, InvalidOperation, localcontext
import re
from typing import List


_REAL_PATTERN = re.compile(
    r"(?<![A-Za-z0-9_])"
    r"(?:"
    r"\d+\.\d+(?:[eE][+-]?\d+)?"
    r"|"
    r"\.\d+(?:[eE][+-]?\d+)?"
    r"|"
    r"\d+[eE][+-]?\d+"
    r")"
    r"(?![A-Za-z0-9_])"
)

_INT_PATTERN = re.compile(r"(?<![A-Za-z0-9_])\d+(?![A-Za-z0-9_])")

_FLOAT_MIN_NONZERO = Decimal("1e-38")
_FLOAT_MAX = Decimal("1e38")


@dataclass(frozen=True)
class NumericConstantViolation:
    line_number: int
    column: int
    message: str


def _leading_unary_sign(statement_text: str, start: int) -> int:
    if start <= 0:
        return 1
    sign_char = statement_text[start - 1]
    if sign_char not in ("+", "-"):
        return 1
    prev = statement_text[start - 2] if start >= 2 else ""
    if prev and (prev.isalnum() or prev == "_"):
        return 1
    return -1 if sign_char == "-" else 1


def _parse_decimal(token_text: str) -> Decimal:
    normalized = token_text
    if normalized.startswith("."):
        normalized = "0" + normalized
    with localcontext() as ctx:
        ctx.prec = 80
        return Decimal(normalized)


def validate_fortran1957_numeric_constants(
    statement_text: str,
    line_number: int,
    column_offset: int,
) -> List[NumericConstantViolation]:
    violations: List[NumericConstantViolation] = []
    consumed: List[tuple[int, int]] = []

    for match in _REAL_PATTERN.finditer(statement_text):
        start, end = match.span()
        consumed.append((start, end))
        token = match.group(0)

        sign = _leading_unary_sign(statement_text, start)
        try:
            value = _parse_decimal(token) * (Decimal("-1") if sign < 0 else Decimal("1"))
        except InvalidOperation:
            violations.append(
                NumericConstantViolation(
                    line_number=line_number,
                    column=column_offset + start,
                    message=(
                        "Invalid floating point constant syntax in "
                        f"{token}"
                    ),
                )
            )
            continue

        abs_value = abs(value)
        if abs_value == 0:
            continue
        if abs_value < _FLOAT_MIN_NONZERO or abs_value > _FLOAT_MAX:
            violations.append(
                NumericConstantViolation(
                    line_number=line_number,
                    column=column_offset + start,
                    message=(
                        "Floating point constant out of range per "
                        "C28-6003 Chapter II: expected 0 or magnitude "
                        "within approximate limits 1e-38 to 1e38, got "
                        f"{token}"
                    ),
                )
            )

    for match in _INT_PATTERN.finditer(statement_text):
        start, end = match.span()
        if any(start >= s and end <= e for (s, e) in consumed):
            continue
        token = match.group(0)

        sign = _leading_unary_sign(statement_text, start)
        digits = token
        if len(digits) > 5:
            violations.append(
                NumericConstantViolation(
                    line_number=line_number,
                    column=column_offset + start,
                    message=(
                        "Fixed point constant out of range per C28-6003 "
                        "Chapter II: expected 1-5 digits and magnitude < 32768, "
                        f"got {token}"
                    ),
                )
            )
            continue

        value = int(digits) * sign
        if abs(value) >= 32768:
            violations.append(
                NumericConstantViolation(
                    line_number=line_number,
                    column=column_offset + start,
                    message=(
                        "Fixed point constant out of range per C28-6003 "
                        "Chapter II: expected magnitude < 32768, got "
                        f"{value}"
                    ),
                )
            )

    return violations
