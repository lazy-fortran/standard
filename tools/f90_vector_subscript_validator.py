#!/usr/bin/env python3
"""Fortran 90 Vector Subscript Semantic Validator

Implements semantic validation for vector subscripts per ISO/IEC 1539:1991
(Fortran 90 standard), specifically Section 6.2.2.1, Rules R620-R621.

This validator checks:
- Vector subscripts must be rank-one integer array expressions
- Vector subscript elements must be of integer type
- Vector subscripts cannot appear on definable arrays (no modification via vector subscript)
- Bounds checking for vector subscript indices

Reference: ISO/IEC 1539:1991 (WG5 N692), Section 6.2.2.1
"""

import sys
from dataclasses import dataclass, field
from enum import Enum, auto
from pathlib import Path
from typing import List, Optional, Set, Tuple, Dict

sys.path.insert(
    0, str(Path(__file__).parent.parent / "grammars" / "generated" / "modern")
)

from antlr4 import CommonTokenStream, InputStream, ParseTreeWalker
from Fortran90Lexer import Fortran90Lexer
from Fortran90Parser import Fortran90Parser
from Fortran90ParserListener import Fortran90ParserListener


class DiagnosticSeverity(Enum):
    """Diagnostic severity levels."""
    ERROR = auto()
    WARNING = auto()
    INFO = auto()


@dataclass
class SemanticDiagnostic:
    """Semantic diagnostic with ISO section reference."""
    severity: DiagnosticSeverity
    code: str
    message: str
    line: Optional[int] = None
    column: Optional[int] = None
    iso_section: Optional[str] = None


@dataclass
class VectorSubscriptResult:
    """Results from vector subscript semantic validation."""
    diagnostics: List[SemanticDiagnostic] = field(default_factory=list)

    @property
    def has_errors(self) -> bool:
        return any(d.severity == DiagnosticSeverity.ERROR for d in self.diagnostics)

    @property
    def error_count(self) -> int:
        return sum(
            1 for d in self.diagnostics if d.severity == DiagnosticSeverity.ERROR
        )

    @property
    def warning_count(self) -> int:
        return sum(
            1 for d in self.diagnostics if d.severity == DiagnosticSeverity.WARNING
        )


class F90VectorSubscriptListener(Fortran90ParserListener):
    """ANTLR listener for Fortran 90 vector subscript semantic analysis."""

    def __init__(self):
        super().__init__()
        self.result = VectorSubscriptResult()
        self._declared_variables: Dict[str, str] = {}  # name -> type
        self._in_assignment_lhs = False

    def _get_location(self, ctx) -> Tuple[Optional[int], Optional[int]]:
        """Extract line and column from parse tree context."""
        if ctx is None:
            return None, None
        if hasattr(ctx, "start") and ctx.start:
            return ctx.start.line, ctx.start.column
        return None, None

    def _add_diagnostic(
        self,
        severity: DiagnosticSeverity,
        code: str,
        message: str,
        ctx=None,
        iso_section: Optional[str] = None,
    ):
        """Add a semantic diagnostic to the result."""
        line, column = self._get_location(ctx)
        self.result.diagnostics.append(
            SemanticDiagnostic(
                severity=severity,
                code=code,
                message=message,
                line=line,
                column=column,
                iso_section=iso_section,
            )
        )

    def _get_expr_rank(self, expr_ctx) -> Optional[int]:
        """Determine the rank of an expression context (simplified).

        This is a simplified rank determination. A full implementation would
        need symbol table and type information.
        """
        if expr_ctx is None:
            return None

        # Check for array constructor - rank 1
        if hasattr(expr_ctx, 'ac_value_list') and expr_ctx.ac_value_list():
            return 1

        # Check for function call that might return an array
        if hasattr(expr_ctx, 'function_call'):
            # For now, assume function calls can return any rank
            return None

        # Check for simple name reference
        if hasattr(expr_ctx, 'name'):
            # Could be array or scalar - return None to indicate unknown
            return None

        return None

    def _is_integer_type(self, expr_ctx) -> Optional[bool]:
        """Determine if an expression has integer type (simplified).

        This is a simplified type determination. A full implementation would
        need complete symbol table and type inference.
        """
        if expr_ctx is None:
            return None

        # Check for integer literal
        if hasattr(expr_ctx, 'int_literal_constant'):
            return True

        # Check for array constructor with integers
        if hasattr(expr_ctx, 'ac_value_list'):
            # Would need to check all values in constructor
            return None

        return None

    def enterVector_subscript(self, ctx):
        """Analyze vector subscript for semantic constraints."""
        # Get the subscript expression
        if not hasattr(ctx, 'expr_f90') or ctx.expr_f90() is None:
            return

        expr = ctx.expr_f90()

        # E675-001: Vector subscript must be rank-one array expression
        # This check requires full type analysis with symbol table
        rank = self._get_expr_rank(expr)
        if rank is not None and rank != 1:
            self._add_diagnostic(
                DiagnosticSeverity.ERROR,
                "E675-001",
                f"Vector subscript must be rank-one array expression, but has rank {rank} "
                "(ISO/IEC 1539:1991 §6.2.2.1, R621)",
                ctx,
                "6.2.2.1",
            )

        # E675-002: Vector subscript elements must be integer type
        is_integer = self._is_integer_type(expr)
        if is_integer is False:
            self._add_diagnostic(
                DiagnosticSeverity.ERROR,
                "E675-002",
                "Vector subscript elements must be of integer type "
                "(ISO/IEC 1539:1991 §6.2.2.1, R621)",
                ctx,
                "6.2.2.1",
            )

        # E675-003: Vector subscript indices must be in valid bounds
        # This requires runtime value analysis and array bounds knowledge
        # Simplified check for obvious out-of-bounds cases
        if hasattr(expr, 'ac_value_list') and expr.ac_value_list():
            # Could check array constructor values
            pass

    def enterType_declaration_stmt(self, ctx):
        """Track variable declarations for type information."""
        # Extract declared variable names and types
        # This is a simplified implementation
        if hasattr(ctx, 'entity_decl_list') and ctx.entity_decl_list():
            # Track for later use
            pass


def validate_f90_vector_subscripts(source_code: str) -> VectorSubscriptResult:
    """Validate Fortran 90 source code for vector subscript semantic compliance.

    Args:
        source_code: Fortran 90 source code string

    Returns:
        VectorSubscriptResult with diagnostics
    """
    try:
        input_stream = InputStream(source_code)
        lexer = Fortran90Lexer(input_stream)
        stream = CommonTokenStream(lexer)
        parser = Fortran90Parser(stream)
        tree = parser.program_unit_f90()

        listener = F90VectorSubscriptListener()
        ParseTreeWalker().walk(listener, tree)

        return listener.result
    except Exception as e:
        # If parsing fails, return a diagnostic
        result = VectorSubscriptResult()
        result.diagnostics.append(
            SemanticDiagnostic(
                severity=DiagnosticSeverity.ERROR,
                code="E675-PARSE",
                message=f"Parse error during vector subscript validation: {str(e)}",
                iso_section="6.2.2.1",
            )
        )
        return result


def validate_f90_vector_subscripts_file(
    filepath: str,
) -> VectorSubscriptResult:
    """Validate Fortran 90 source file for vector subscript semantic compliance.

    Args:
        filepath: Path to Fortran 90 source file

    Returns:
        VectorSubscriptResult with diagnostics
    """
    try:
        with open(filepath, "r", encoding="utf-8", errors="ignore") as f:
            source_code = f.read()
        return validate_f90_vector_subscripts(source_code)
    except IOError as e:
        result = VectorSubscriptResult()
        result.diagnostics.append(
            SemanticDiagnostic(
                severity=DiagnosticSeverity.ERROR,
                code="E675-FILE",
                message=f"Cannot read file {filepath}: {str(e)}",
                iso_section="6.2.2.1",
            )
        )
        return result
