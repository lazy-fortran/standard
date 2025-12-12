#!/usr/bin/env python3
"""FORTRAN 66 DIMENSION bounds semantic validator.

Implements semantic validation for array dimension bounds per ANSI X3.9-1966
Section 5.3. The FORTRAN 66 grammar accepts DIMENSION bounds syntactically
via `dimension_bound : expr (COLON expr)?`, but the standard imposes semantic
constraints:

- Single bound form `d` requires a positive integer constant (d > 0).
- Explicit bound form `d1:d2` requires:
  - d1 is a signed integer constant (may be negative).
  - d2 is a positive integer constant (d2 > 0).
  - d2 >= d1.

The same upper-bound positivity and integer-constant requirements apply to
array declarators in type declarations.
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
from FORTRAN66Lexer import FORTRAN66Lexer
from FORTRAN66Parser import FORTRAN66Parser
from FORTRAN66ParserListener import FORTRAN66ParserListener


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
class DimensionValidationResult:
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


_INT_LITERAL_RE = re.compile(r"[+-]?\d+")


def _strip_outer_parens(text: str) -> str:
    stripped = text.strip()
    while stripped.startswith("(") and stripped.endswith(")"):
        inner = stripped[1:-1].strip()
        if inner.count("(") != inner.count(")"):
            break
        stripped = inner
    return stripped


def _parse_int_constant(text: str) -> Optional[int]:
    cleaned = _strip_outer_parens(text.replace(" ", ""))
    if _INT_LITERAL_RE.fullmatch(cleaned):
        return int(cleaned)
    return None


def _get_location(ctx) -> Tuple[Optional[int], Optional[int]]:
    if ctx is None or not hasattr(ctx, "start") or ctx.start is None:
        return None, None
    return ctx.start.line, ctx.start.column


class F66DimensionBoundsListener(FORTRAN66ParserListener):
    """ANTLR listener enforcing X3.9-1966 DIMENSION bound semantics."""

    def __init__(self):
        super().__init__()
        self.result = DimensionValidationResult()
        self._in_type_declaration = False

    def enterType_declaration(self, ctx):
        self._in_type_declaration = True

    def exitType_declaration(self, ctx):
        self._in_type_declaration = False

    def exitArray_declarator(self, ctx):
        # Array declarators inside DIMENSION statements.
        name = ctx.IDENTIFIER().getText() if ctx.IDENTIFIER() else "<unknown>"
        dim_list = ctx.dimension_list()
        if dim_list is None:
            return
        for bound_ctx in dim_list.dimension_bound():
            self._validate_dimension_bound(bound_ctx, name)

    def exitVariable(self, ctx):
        # Array declarators inside type declarations use the inherited variable rule.
        if not self._in_type_declaration:
            return
        expr_list_ctx = ctx.expr_list() if hasattr(ctx, "expr_list") else None
        if expr_list_ctx is None:
            return
        for expr_ctx in expr_list_ctx.expr():
            self._validate_single_upper_bound(expr_ctx, ctx.IDENTIFIER().getText())

    def _add_error(self, code: str, message: str, ctx):
        line, column = _get_location(ctx)
        self.result.diagnostics.append(
            SemanticDiagnostic(
                severity=DiagnosticSeverity.ERROR,
                code=code,
                message=message,
                line=line,
                column=column,
                iso_section="ANSI X3.9-1966 Section 5.3",
            )
        )

    def _validate_single_upper_bound(self, expr_ctx, array_name: str):
        text = expr_ctx.getText()
        value = _parse_int_constant(text)
        if value is None:
            self._add_error(
                "DIM66_E001",
                f"Array '{array_name}' upper bound '{text}' must be an integer constant.",
                expr_ctx,
            )
            return
        if value <= 0:
            self._add_error(
                "DIM66_E002",
                f"Array '{array_name}' upper bound {value} must be positive.",
                expr_ctx,
            )

    def _validate_dimension_bound(self, bound_ctx, array_name: str):
        exprs = bound_ctx.expr()
        if not exprs:
            return
        lower_ctx = exprs[0]
        upper_ctx = exprs[1] if len(exprs) > 1 else None

        if upper_ctx is None:
            self._validate_single_upper_bound(lower_ctx, array_name)
            return

        lower_text = lower_ctx.getText()
        upper_text = upper_ctx.getText()
        lower_val = _parse_int_constant(lower_text)
        upper_val = _parse_int_constant(upper_text)

        if lower_val is None:
            self._add_error(
                "DIM66_E001",
                f"Array '{array_name}' lower bound '{lower_text}' must be an integer constant.",
                lower_ctx,
            )

        if upper_val is None:
            self._add_error(
                "DIM66_E001",
                f"Array '{array_name}' upper bound '{upper_text}' must be an integer constant.",
                upper_ctx,
            )
            return

        if upper_val <= 0:
            self._add_error(
                "DIM66_E002",
                f"Array '{array_name}' upper bound {upper_val} must be positive.",
                upper_ctx,
            )

        if lower_val is not None and upper_val < lower_val:
            self._add_error(
                "DIM66_E003",
                f"Array '{array_name}' upper bound {upper_val} must be >= lower bound {lower_val}.",
                upper_ctx,
            )


class F66DimensionBoundsValidator:
    """Semantic validator for FORTRAN 66 array bounds."""

    def validate_code(self, code: str) -> DimensionValidationResult:
        input_stream = InputStream(code)
        lexer = FORTRAN66Lexer(input_stream)
        token_stream = CommonTokenStream(lexer)
        parser = FORTRAN66Parser(token_stream)

        tree = parser.fortran66_program()
        syntax_errors = parser.getNumberOfSyntaxErrors()

        result = DimensionValidationResult()
        if syntax_errors > 0:
            result.diagnostics.append(
                SemanticDiagnostic(
                    severity=DiagnosticSeverity.ERROR,
                    code="SYNTAX_E001",
                    message=(
                        f"Code contains {syntax_errors} syntax error(s). "
                        "Fix syntax errors before semantic validation."
                    ),
                )
            )
            return result

        listener = F66DimensionBoundsListener()
        ParseTreeWalker().walk(listener, tree)
        return listener.result

    def validate_file(self, filepath: str) -> DimensionValidationResult:
        path = Path(filepath)
        if not path.exists():
            result = DimensionValidationResult()
            result.diagnostics.append(
                SemanticDiagnostic(
                    severity=DiagnosticSeverity.ERROR,
                    code="FILE_E001",
                    message=f"File not found: {filepath}",
                )
            )
            return result
        return self.validate_code(path.read_text())


def validate_dimension_bounds_66(code: str) -> DimensionValidationResult:
    """Convenience function for validating FORTRAN 66 array bounds."""
    return F66DimensionBoundsValidator().validate_code(code)

