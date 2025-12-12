#!/usr/bin/env python3
"""Fortran 90 CYCLE/EXIT Semantic Validator

Implements semantic validation for Fortran 90 CYCLE/EXIT statements per
ISO/IEC 1539:1991 (Fortran 90 standard), specifically section 8.1.4.4.

This validator checks:
- CYCLE and EXIT statement placement within DO constructs
- Construct-name matching and binding
- Proper scoping and nesting of named constructs

Reference: ISO/IEC 1539:1991 (WG5 N692), Section 8.1.4.4
"""

import sys
from dataclasses import dataclass, field
from enum import Enum, auto
from pathlib import Path
from typing import List, Optional, Set, Tuple

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
class DoConstruct:
    """Represents a DO construct for semantic analysis."""
    name: Optional[str] = None  # Construct name (if present)
    line: Optional[int] = None
    column: Optional[int] = None
    depth: int = 0


@dataclass
class CycleExitValidationResult:
    """Results from CYCLE/EXIT semantic validation."""
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


class F90CycleExitListener(Fortran90ParserListener):
    """ANTLR listener for Fortran 90 CYCLE/EXIT semantic analysis."""

    def __init__(self):
        super().__init__()
        self.result = CycleExitValidationResult()
        self._do_construct_stack: List[DoConstruct] = []
        self._named_constructs: Set[str] = set()

    def _get_token_text(self, ctx) -> str:
        """Extract and normalize token text."""
        if ctx is None:
            return ""
        # Handle both token objects and parse tree contexts
        text = ctx.getText() if hasattr(ctx, 'getText') else str(ctx)
        return text.lower()

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

    def enterDo_construct_f90(self, ctx):
        """Enter a DO construct and track its name (if present)."""
        construct_name = None

        # Check for a do_stmt_f90 with optional construct name
        if hasattr(ctx, "do_stmt_f90") and ctx.do_stmt_f90():
            do_stmt = ctx.do_stmt_f90()
            # do_stmt_f90 grammar: (IDENTIFIER COLON)? DO (label_f90)? (loop_control)?
            # Extract IDENTIFIER if present
            if hasattr(do_stmt, "IDENTIFIER"):
                identifier_token = do_stmt.IDENTIFIER()
                if identifier_token:
                    construct_name = self._get_token_text(identifier_token)

        depth = len(self._do_construct_stack)
        do_construct = DoConstruct(
            name=construct_name,
            depth=depth,
            line=self._get_location(ctx)[0],
            column=self._get_location(ctx)[1],
        )
        self._do_construct_stack.append(do_construct)

        if construct_name:
            self._named_constructs.add(construct_name)

    def exitDo_construct_f90(self, ctx):
        """Exit a DO construct."""
        if self._do_construct_stack:
            do_construct = self._do_construct_stack.pop()
            if do_construct.name:
                self._named_constructs.discard(do_construct.name)

    def enterCycle_stmt(self, ctx):
        """Enter a CYCLE statement and validate its placement/binding."""
        # CYCLE [construct-name]
        construct_name = None
        if hasattr(ctx, "IDENTIFIER"):
            identifier_token = ctx.IDENTIFIER()
            if identifier_token:
                construct_name = self._get_token_text(identifier_token)

        # Check if CYCLE is inside a DO construct
        if not self._do_construct_stack:
            self._add_diagnostic(
                DiagnosticSeverity.ERROR,
                "E001",
                "CYCLE statement outside any DO construct (ISO/IEC 1539:1991 §8.1.4.4.3)",
                ctx,
                "8.1.4.4.3",
            )
            return

        if construct_name:
            # CYCLE with a construct name: must match an active named DO construct
            found = False
            for do_construct in reversed(self._do_construct_stack):
                if do_construct.name == construct_name:
                    found = True
                    break

            if not found:
                self._add_diagnostic(
                    DiagnosticSeverity.ERROR,
                    "E002",
                    f"CYCLE construct name '{construct_name}' does not match "
                    f"any active DO construct (ISO/IEC 1539:1991 §8.1.4.4.3)",
                    ctx,
                    "8.1.4.4.3",
                )

    def enterExit_stmt(self, ctx):
        """Enter an EXIT statement and validate its placement/binding."""
        # EXIT [construct-name]
        construct_name = None
        if hasattr(ctx, "IDENTIFIER"):
            identifier_token = ctx.IDENTIFIER()
            if identifier_token:
                construct_name = self._get_token_text(identifier_token)

        # Check if EXIT is inside a DO construct
        if not self._do_construct_stack:
            self._add_diagnostic(
                DiagnosticSeverity.ERROR,
                "E003",
                "EXIT statement outside any DO construct (ISO/IEC 1539:1991 §8.1.4.4.4)",
                ctx,
                "8.1.4.4.4",
            )
            return

        if construct_name:
            # EXIT with a construct name: must match an active named DO construct
            found = False
            for do_construct in reversed(self._do_construct_stack):
                if do_construct.name == construct_name:
                    found = True
                    break

            if not found:
                self._add_diagnostic(
                    DiagnosticSeverity.ERROR,
                    "E004",
                    f"EXIT construct name '{construct_name}' does not match "
                    f"any active DO construct (ISO/IEC 1539:1991 §8.1.4.4.4)",
                    ctx,
                    "8.1.4.4.4",
                )


def validate_f90_cycle_exit(source_code: str) -> CycleExitValidationResult:
    """Validate Fortran 90 source code for CYCLE/EXIT semantic compliance.

    Args:
        source_code: Fortran 90 source code string

    Returns:
        CycleExitValidationResult with diagnostics
    """
    try:
        input_stream = InputStream(source_code)
        lexer = Fortran90Lexer(input_stream)
        stream = CommonTokenStream(lexer)
        parser = Fortran90Parser(stream)
        tree = parser.program_unit_f90()

        listener = F90CycleExitListener()
        walker = ParseTreeWalker()
        walker.walk(listener, tree)

        return listener.result
    except Exception as e:
        result = CycleExitValidationResult()
        result.diagnostics.append(
            SemanticDiagnostic(
                severity=DiagnosticSeverity.ERROR,
                code="PARSE_ERROR",
                message=f"Failed to parse source code: {str(e)}",
            )
        )
        return result


def main():
    """Command-line interface for the Fortran 90 CYCLE/EXIT validator."""
    if len(sys.argv) < 2:
        print("Usage: f90_cycle_exit_validator.py <source_file>")
        sys.exit(1)

    source_file = sys.argv[1]
    try:
        with open(source_file, "r") as f:
            source_code = f.read()
    except IOError as e:
        print(f"Error reading file: {e}", file=sys.stderr)
        sys.exit(1)

    result = validate_f90_cycle_exit(source_code)

    if result.diagnostics:
        print(f"Found {result.error_count} error(s), {result.warning_count} warning(s):")
        for diag in result.diagnostics:
            severity_str = diag.severity.name
            location = ""
            if diag.line:
                location = f" at line {diag.line}"
            iso_str = ""
            if diag.iso_section:
                iso_str = f" (ISO §{diag.iso_section})"
            print(
                f"  [{severity_str}] {diag.code}: {diag.message}{location}{iso_str}"
            )
    else:
        print("✓ No CYCLE/EXIT semantic violations found")

    sys.exit(0 if not result.has_errors else 1)


if __name__ == "__main__":
    main()
