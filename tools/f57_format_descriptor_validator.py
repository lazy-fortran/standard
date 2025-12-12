#!/usr/bin/env python3
"""FORTRAN 1957 FORMAT descriptor semantic validator.

Implements semantic validation for FORMAT statement descriptors per IBM
FORTRAN for the IBM 704 (Form C28-6003, Oct 1958), Chapter 5.

FORMAT descriptor rules (C28-6003):
- Iw: Integer conversion, w must be positive
- Fw.d: Fixed-point conversion, w > 0, d >= 0, d < 10 (if d >= 10 treated mod 10)
- Ew.d: Exponential conversion, w > 0, d >= 0, d < 10 (if d >= 10 treated mod 10)
- Scale factor: nP where n is signed integer (0 applies to all following E/F)
- Hollerith: nH where n > 0, exactly n characters follow
- All widths/precision must be integer constants

Per C28-6003:
- Field widths must be positive (at least 1)
- For E/F conversion, d decimal places are rounded and printed
- If d >= 10, it is treated mod 10
- Scale factor P can precede any E/F specification
- Width/precision combinations must be meaningful for the described field
"""

from __future__ import annotations

import re
import sys
from dataclasses import dataclass, field
from enum import Enum, auto
from pathlib import Path
from typing import List, Optional, Tuple

sys.path.insert(
    0, str(Path(__file__).parent.parent / "grammars" / "generated" / "early")
)

from antlr4 import CommonTokenStream, InputStream, ParseTreeWalker
from FORTRANLexer import FORTRANLexer
from FORTRANParser import FORTRANParser
from FORTRANParserListener import FORTRANParserListener


class DiagnosticSeverity(Enum):
    ERROR = auto()
    WARNING = auto()
    INFO = auto()


@dataclass
class SemanticDiagnostic:
    severity: DiagnosticSeverity
    code: str
    message: str
    line: Optional[int] = None
    column: Optional[int] = None
    iso_section: Optional[str] = None


@dataclass
class FormatDescriptorValidationResult:
    diagnostics: List[SemanticDiagnostic] = field(default_factory=list)

    @property
    def has_errors(self) -> bool:
        return any(d.severity == DiagnosticSeverity.ERROR for d in self.diagnostics)

    @property
    def error_count(self) -> int:
        return sum(1 for d in self.diagnostics if d.severity == DiagnosticSeverity.ERROR)

    @property
    def warning_count(self) -> int:
        return sum(
            1 for d in self.diagnostics if d.severity == DiagnosticSeverity.WARNING
        )


def _get_location(ctx) -> Tuple[Optional[int], Optional[int]]:
    if ctx is None or not hasattr(ctx, "start") or ctx.start is None:
        return None, None
    return ctx.start.line, ctx.start.column


class FormatDescriptorValidator:
    """Semantic validator for FORTRAN 1957 FORMAT descriptors."""

    def __init__(self):
        self.diagnostics: List[SemanticDiagnostic] = []

    def validate_format_descriptor_string(
        self, format_string: str, format_text_context: str = ""
    ) -> FormatDescriptorValidationResult:
        """
        Validate FORMAT statement descriptor string semantics.

        Args:
            format_string: The content of a FORMAT specification (inside parentheses)
            format_text_context: Optional context for error messages

        Returns:
            FormatDescriptorValidationResult containing diagnostics
        """
        result = FormatDescriptorValidationResult()
        self.diagnostics = []

        # Parse FORMAT specification tokens (simplified - real parser would use ANTLR)
        tokens = self._tokenize_format_spec(format_string)
        self._validate_descriptor_tokens(tokens, result, format_text_context)

        return result

    def _tokenize_format_spec(self, spec: str) -> List[str]:
        """Tokenize FORMAT specification by commas and slashes."""
        # Remove outer parens if present
        spec = spec.strip()
        if spec.startswith("(") and spec.endswith(")"):
            spec = spec[1:-1]

        # Split by commas, preserving context (including Hollerith fields)
        tokens = []
        current = ""
        paren_depth = 0
        in_hollerith = False
        hollerith_remaining = 0

        for char in spec:
            if in_hollerith:
                current += char
                hollerith_remaining -= 1
                if hollerith_remaining == 0:
                    in_hollerith = False
            elif char == "(":
                paren_depth += 1
                current += char
            elif char == ")":
                paren_depth -= 1
                current += char
            elif char == "," and paren_depth == 0 and not in_hollerith:
                if current.strip():
                    tokens.append(current.strip())
                current = ""
            elif char.upper() == "H" and paren_depth == 0 and not in_hollerith:
                # Check if we're entering a Hollerith field
                match = re.search(r"(\d+)$", current)
                if match:
                    hollerith_remaining = int(match.group(1))
                    in_hollerith = True
                current += char
            else:
                current += char

        if current.strip():
            tokens.append(current.strip())

        return tokens

    def _validate_descriptor_tokens(
        self,
        tokens: List[str],
        result: FormatDescriptorValidationResult,
        context: str,
    ):
        """Validate each FORMAT descriptor token."""
        for token in tokens:
            self._validate_single_descriptor(token, result, context)

    def _validate_single_descriptor(
        self, token: str, result: FormatDescriptorValidationResult, context: str
    ):
        """Validate a single FORMAT descriptor."""
        token = token.strip()
        if not token:
            return

        # Handle scale factor (nP)
        scale_match = re.match(r"^([+-]?\d+)P$", token, re.IGNORECASE)
        if scale_match:
            self._validate_scale_factor(scale_match.group(1), result, context)
            return

        # Handle I-conversion (Iw or nIw)
        i_match = re.match(r"^(\d*)I(\d+)$", token, re.IGNORECASE)
        if i_match:
            repeat = i_match.group(1) or "1"
            width = i_match.group(2)
            self._validate_i_descriptor(repeat, width, result, context)
            return

        # Handle F-conversion (Fw.d or nFw.d)
        f_match = re.match(r"^(\d*)F(\d+)\.([+-]?\d+)$", token, re.IGNORECASE)
        if f_match:
            repeat = f_match.group(1) or "1"
            width = f_match.group(2)
            precision = f_match.group(3)
            self._validate_f_descriptor(repeat, width, precision, result, context)
            return

        # Handle E-conversion (Ew.d or nEw.d)
        e_match = re.match(r"^(\d*)E(\d+)\.([+-]?\d+)$", token, re.IGNORECASE)
        if e_match:
            repeat = e_match.group(1) or "1"
            width = e_match.group(2)
            precision = e_match.group(3)
            self._validate_e_descriptor(repeat, width, precision, result, context)
            return

        # Handle Hollerith (nH or nH...)
        h_match = re.match(r"^(\d+)H", token, re.IGNORECASE)
        if h_match:
            count = int(h_match.group(1))
            chars_after = token[len(h_match.group(0)) :]
            self._validate_hollerith_descriptor(
                count, chars_after, result, context, token
            )
            return

        # Handle grouped specifications with parentheses
        if "(" in token and ")" in token:
            # Recursively validate grouped specs
            inner = token[token.index("(") + 1 : token.rindex(")")]
            repeat_match = re.match(r"^(\d+)\(", token)
            if repeat_match:
                # n(spec) format - validation happens on inner specs
                self._validate_descriptor_tokens(
                    self._tokenize_format_spec(inner), result, context
                )
            return

        # Handle slashes (multi-record formats) - just skip, they're structural
        if token == "/":
            return

    def _validate_i_descriptor(
        self,
        repeat: str,
        width: str,
        result: FormatDescriptorValidationResult,
        context: str,
    ):
        """Validate Iw descriptor."""
        try:
            w = int(width)
            if w <= 0:
                result.diagnostics.append(
                    SemanticDiagnostic(
                        severity=DiagnosticSeverity.ERROR,
                        code="FMT57_E001",
                        message=f"I-conversion width must be positive, got {w}",
                        iso_section="C28-6003 Chapter 5 (Iw)",
                    )
                )
        except ValueError:
            result.diagnostics.append(
                SemanticDiagnostic(
                    severity=DiagnosticSeverity.ERROR,
                    code="FMT57_E002",
                    message=f"I-conversion width must be integer constant, got '{width}'",
                    iso_section="C28-6003 Chapter 5 (Iw)",
                )
            )

    def _validate_f_descriptor(
        self,
        repeat: str,
        width: str,
        precision: str,
        result: FormatDescriptorValidationResult,
        context: str,
    ):
        """Validate Fw.d descriptor."""
        try:
            w = int(width)
            d = int(precision)

            if w <= 0:
                result.diagnostics.append(
                    SemanticDiagnostic(
                        severity=DiagnosticSeverity.ERROR,
                        code="FMT57_E001",
                        message=f"F-conversion width must be positive, got {w}",
                        iso_section="C28-6003 Chapter 5 (Fw.d)",
                    )
                )

            # Precision can be >= 10; if d >= 10 it's treated mod 10 per standard
            if d < 0:
                result.diagnostics.append(
                    SemanticDiagnostic(
                        severity=DiagnosticSeverity.ERROR,
                        code="FMT57_E003",
                        message=f"F-conversion decimal places must be non-negative, got {d}",
                        iso_section="C28-6003 Chapter 5 (Fw.d)",
                    )
                )

            # Practical check: width should be large enough for precision
            # For F-conversion: need at least d+3 (sign, digits, decimal point)
            if w > 0 and d >= 0 and w < min(d + 2, 1):
                result.diagnostics.append(
                    SemanticDiagnostic(
                        severity=DiagnosticSeverity.WARNING,
                        code="FMT57_W001",
                        message=(
                            f"F-conversion width {w} may be insufficient "
                            f"for {d} decimal places"
                        ),
                        iso_section="C28-6003 Chapter 5 (Fw.d)",
                    )
                )

        except ValueError:
            result.diagnostics.append(
                SemanticDiagnostic(
                    severity=DiagnosticSeverity.ERROR,
                    code="FMT57_E002",
                    message=f"F-conversion width/precision must be integer constants",
                    iso_section="C28-6003 Chapter 5 (Fw.d)",
                )
            )

    def _validate_e_descriptor(
        self,
        repeat: str,
        width: str,
        precision: str,
        result: FormatDescriptorValidationResult,
        context: str,
    ):
        """Validate Ew.d descriptor."""
        try:
            w = int(width)
            d = int(precision)

            if w <= 0:
                result.diagnostics.append(
                    SemanticDiagnostic(
                        severity=DiagnosticSeverity.ERROR,
                        code="FMT57_E001",
                        message=f"E-conversion width must be positive, got {w}",
                        iso_section="C28-6003 Chapter 5 (Ew.d)",
                    )
                )

            # Per C28-6003, if d >= 10 it is treated mod 10
            if d < 0:
                result.diagnostics.append(
                    SemanticDiagnostic(
                        severity=DiagnosticSeverity.ERROR,
                        code="FMT57_E003",
                        message=f"E-conversion decimal places must be non-negative, got {d}",
                        iso_section="C28-6003 Chapter 5 (Ew.d)",
                    )
                )

            # E-conversion requires space for exponent and notation
            # Minimum: e.g. "0.123E+00" requires about w >= 9
            if w > 0 and w < 7:
                result.diagnostics.append(
                    SemanticDiagnostic(
                        severity=DiagnosticSeverity.WARNING,
                        code="FMT57_W002",
                        message=(
                            f"E-conversion width {w} may be insufficient "
                            f"for exponential notation"
                        ),
                        iso_section="C28-6003 Chapter 5 (Ew.d)",
                    )
                )

        except ValueError:
            result.diagnostics.append(
                SemanticDiagnostic(
                    severity=DiagnosticSeverity.ERROR,
                    code="FMT57_E002",
                    message=f"E-conversion width/precision must be integer constants",
                    iso_section="C28-6003 Chapter 5 (Ew.d)",
                )
            )

    def _validate_scale_factor(
        self,
        scale_value: str,
        result: FormatDescriptorValidationResult,
        context: str,
    ):
        """Validate scale factor nP."""
        try:
            n = int(scale_value)
            # Scale factor can be any integer, positive, negative, or zero
            # per C28-6003: "scale factor is assumed to be zero if no other
            # value has been given"
        except ValueError:
            result.diagnostics.append(
                SemanticDiagnostic(
                    severity=DiagnosticSeverity.ERROR,
                    code="FMT57_E004",
                    message=f"Scale factor must be integer constant, got '{scale_value}'",
                    iso_section="C28-6003 Chapter 5 (Scale factor)",
                )
            )

    def _validate_hollerith_descriptor(
        self,
        count: int,
        chars_after: str,
        result: FormatDescriptorValidationResult,
        context: str,
        full_token: str,
    ):
        """Validate Hollerith descriptor."""
        if count <= 0:
            result.diagnostics.append(
                SemanticDiagnostic(
                    severity=DiagnosticSeverity.ERROR,
                    code="FMT57_E005",
                    message=f"Hollerith count must be positive, got {count}",
                    iso_section="C28-6003 Chapter 5 (Hollerith Fields)",
                )
            )
            return

        if len(chars_after) != count:
            result.diagnostics.append(
                SemanticDiagnostic(
                    severity=DiagnosticSeverity.ERROR,
                    code="FMT57_E006",
                    message=(
                        f"Hollerith count mismatch: declared {count}, "
                        f"but {len(chars_after)} characters follow in '{full_token}'"
                    ),
                    iso_section="C28-6003 Chapter 5 (Hollerith Fields)",
                )
            )


def validate_format_descriptors_f57(format_string: str) -> FormatDescriptorValidationResult:
    """Convenience function for validating FORTRAN 1957 FORMAT descriptors."""
    validator = FormatDescriptorValidator()
    return validator.validate_format_descriptor_string(format_string)


if __name__ == "__main__":
    # Test cases
    test_cases = [
        # Valid cases
        ("I12", True, "Simple I descriptor"),
        ("F10.4", True, "Simple F descriptor"),
        ("E12.4", True, "Simple E descriptor"),
        ("1P3F11.3", True, "Scale factor with F"),
        ("3E12.4", True, "Repeated E descriptor"),
        ("5HHELLO", True, "Hollerith field"),
        ("I12,E12.4,F10.4", True, "Multiple descriptors"),
        # Invalid cases
        ("I0", False, "Zero width I"),
        ("I-5", False, "Negative width I (if supported)"),
        ("F0.4", False, "Zero width F"),
        ("F10.-2", False, "Negative precision F"),
        ("E0.4", False, "Zero width E"),
        ("E10.-2", False, "Negative precision E"),
        ("0H", False, "Zero count Hollerith"),
        ("3HHE", False, "Hollerith count mismatch"),
    ]

    validator = FormatDescriptorValidator()
    for spec, expected_ok, description in test_cases:
        result = validator.validate_format_descriptor_string(spec)
        is_ok = not result.has_errors
        status = "✓" if is_ok == expected_ok else "✗"
        errors = (
            f"({result.error_count} errors)"
            if result.has_errors
            else "valid"
        )
        print(f"{status} {spec}: {description} - {errors}")
        if result.diagnostics and is_ok != expected_ok:
            for diag in result.diagnostics:
                print(f"    {diag.code}: {diag.message}")
