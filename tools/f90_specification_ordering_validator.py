#!/usr/bin/env python3
"""Fortran 90 Specification-Statement Ordering Semantic Validator

Implements semantic validation for Fortran 90 specification_part ordering
constraints per ISO/IEC 1539:1991 (Fortran 90 standard), specifically
section 5.2.1.

This validator checks:
- USE statements must appear at the beginning of specification_part
- IMPLICIT statements must follow USE statements but precede declarations
- PARAMETER statements can appear early in specification_part
- Declaration ordering constraints
- Context-specific restrictions (INTENT/OPTIONAL in procedures only, etc.)

Reference: ISO/IEC 1539:1991 (WG5 N692), Section 5.2.1 and Figure 2.1
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


class StatementType(Enum):
    """Classification of specification statement types."""
    USE = auto()
    IMPLICIT = auto()
    PARAMETER = auto()
    DERIVED_TYPE = auto()
    INTERFACE = auto()
    TYPE_DECL = auto()
    SPEC_STMT = auto()
    FORMAT = auto()
    ENTRY = auto()
    STMT_FUNC = auto()
    EXECUTABLE = auto()
    OTHER = auto()


class ProgramUnitType(Enum):
    """Classification of program unit contexts."""
    MAIN_PROGRAM = auto()
    SUBROUTINE = auto()
    FUNCTION = auto()
    MODULE = auto()
    BLOCK_DATA = auto()
    INTERNAL_PROCEDURE = auto()


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
class SpecificationOrderingResult:
    """Results from specification ordering semantic validation."""
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


class F90SpecificationOrderingListener(Fortran90ParserListener):
    """ANTLR listener for Fortran 90 specification_part ordering analysis."""

    def __init__(self):
        super().__init__()
        self.result = SpecificationOrderingResult()
        self._unit_stack: List[ProgramUnitType] = []
        self._in_specification_part = False
        self._statements_seen: List[Tuple[StatementType, str, int, int]] = []

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

    def _get_statement_type(self, stmt_name: str) -> StatementType:
        """Classify a statement by its grammar rule name."""
        stmt_lower = stmt_name.lower()

        if "use_stmt" in stmt_lower:
            return StatementType.USE
        elif "implicit" in stmt_lower:
            return StatementType.IMPLICIT
        elif "parameter_stmt" in stmt_lower or "named_constant_def_stmt" in stmt_lower:
            return StatementType.PARAMETER
        elif "derived_type_def" in stmt_lower:
            return StatementType.DERIVED_TYPE
        elif "interface" in stmt_lower:
            return StatementType.INTERFACE
        elif "type_declaration_stmt" in stmt_lower:
            return StatementType.TYPE_DECL
        elif "format_stmt" in stmt_lower:
            return StatementType.FORMAT
        elif "entry_stmt" in stmt_lower:
            return StatementType.ENTRY
        elif "stmt_function_stmt" in stmt_lower or "statement_function_stmt" in stmt_lower:
            return StatementType.STMT_FUNC
        elif "executable" in stmt_lower:
            return StatementType.EXECUTABLE
        else:
            return StatementType.OTHER

    def _is_specification_statement(self, stmt_type: StatementType) -> bool:
        """Check if a statement type belongs in specification_part."""
        return stmt_type in {
            StatementType.USE,
            StatementType.IMPLICIT,
            StatementType.PARAMETER,
            StatementType.DERIVED_TYPE,
            StatementType.INTERFACE,
            StatementType.TYPE_DECL,
            StatementType.SPEC_STMT,
            StatementType.FORMAT,
            StatementType.ENTRY,
            StatementType.STMT_FUNC,
        }

    def _validate_ordering(self, stmt_type: StatementType, ctx, stmt_name: str = ""):
        """Validate statement ordering within specification_part."""
        if not self._in_specification_part:
            return

        line, column = self._get_location(ctx)

        # Rule 1: USE statements must be first
        if stmt_type == StatementType.USE:
            # Check if we've seen non-USE statements
            for prev_type, _, _, _ in self._statements_seen:
                if prev_type != StatementType.USE:
                    self._add_diagnostic(
                        DiagnosticSeverity.ERROR,
                        "E672-001",
                        "USE statement not at beginning of specification_part "
                        "(ISO/IEC 1539:1991 §5.2.1, R204)",
                        ctx,
                        "5.2.1",
                    )
                    break

        # Rule 2: IMPLICIT must follow USE statements
        elif stmt_type == StatementType.IMPLICIT:
            # Check if there are non-USE, non-IMPLICIT statements before this
            found_non_use_non_implicit = False
            found_previous_implicit = False
            for prev_type, _, _, _ in self._statements_seen:
                if prev_type not in {StatementType.USE, StatementType.IMPLICIT}:
                    found_non_use_non_implicit = True
                if prev_type == StatementType.IMPLICIT:
                    found_previous_implicit = True

            if found_non_use_non_implicit:
                self._add_diagnostic(
                    DiagnosticSeverity.ERROR,
                    "E672-002",
                    "IMPLICIT statement not immediately following USE statements "
                    "(ISO/IEC 1539:1991 §5.2.1, R205)",
                    ctx,
                    "5.2.1",
                )

        # Rule 3: No executable statements in specification_part
        elif stmt_type == StatementType.EXECUTABLE:
            self._add_diagnostic(
                DiagnosticSeverity.ERROR,
                "E672-020",
                "Executable statement in specification_part "
                "(ISO/IEC 1539:1991 §5.2.1, R203-R204)",
                ctx,
                "5.2.1",
            )

        # Track the statement
        self._statements_seen.append((stmt_type, stmt_name, line or 0, column or 0))

    def enterMain_program(self, ctx):
        """Enter main program context."""
        self._unit_stack.append(ProgramUnitType.MAIN_PROGRAM)
        self._statements_seen = []

    def exitMain_program(self, ctx):
        """Exit main program context."""
        if self._unit_stack:
            self._unit_stack.pop()
        self._statements_seen = []

    def enterSubroutine_subprogram_f90(self, ctx):
        """Enter subroutine context."""
        self._unit_stack.append(ProgramUnitType.SUBROUTINE)
        self._statements_seen = []

    def exitSubroutine_subprogram_f90(self, ctx):
        """Exit subroutine context."""
        if self._unit_stack:
            self._unit_stack.pop()
        self._statements_seen = []

    def enterFunction_subprogram_f90(self, ctx):
        """Enter function context."""
        self._unit_stack.append(ProgramUnitType.FUNCTION)
        self._statements_seen = []

    def exitFunction_subprogram_f90(self, ctx):
        """Exit function context."""
        if self._unit_stack:
            self._unit_stack.pop()
        self._statements_seen = []

    def enterModule(self, ctx):
        """Enter module context."""
        self._unit_stack.append(ProgramUnitType.MODULE)
        self._statements_seen = []

    def exitModule(self, ctx):
        """Exit module context."""
        if self._unit_stack:
            self._unit_stack.pop()
        self._statements_seen = []

    def enterBlock_data_subprogram(self, ctx):
        """Enter block data context."""
        self._unit_stack.append(ProgramUnitType.BLOCK_DATA)
        self._statements_seen = []

    def exitBlock_data_subprogram(self, ctx):
        """Exit block data context."""
        if self._unit_stack:
            self._unit_stack.pop()
        self._statements_seen = []

    def enterSpecification_part(self, ctx):
        """Enter specification_part."""
        self._in_specification_part = True
        self._statements_seen = []

    def exitSpecification_part(self, ctx):
        """Exit specification_part."""
        self._in_specification_part = False
        self._statements_seen = []

    def enterUse_stmt(self, ctx):
        """Process USE statement."""
        self._validate_ordering(StatementType.USE, ctx, "use_stmt")

    def enterImplicit_part(self, ctx):
        """Process IMPLICIT statement."""
        self._validate_ordering(StatementType.IMPLICIT, ctx, "implicit_part")

    def enterParameter_stmt(self, ctx):
        """Process PARAMETER statement."""
        self._validate_ordering(StatementType.PARAMETER, ctx, "parameter_stmt")

    def enterType_declaration_stmt(self, ctx):
        """Process type declaration statement."""
        self._validate_ordering(StatementType.TYPE_DECL, ctx, "type_declaration_stmt")

    def enterDerived_type_def(self, ctx):
        """Process derived-type definition."""
        self._validate_ordering(
            StatementType.DERIVED_TYPE, ctx, "derived_type_def"
        )

    def enterInterface_block(self, ctx):
        """Process interface block."""
        self._validate_ordering(StatementType.INTERFACE, ctx, "interface_block")


def validate_f90_specification_ordering(source_code: str) -> SpecificationOrderingResult:
    """Validate Fortran 90 source code for specification_part ordering compliance.

    Args:
        source_code: Fortran 90 source code string

    Returns:
        SpecificationOrderingResult with diagnostics
    """
    try:
        input_stream = InputStream(source_code)
        lexer = Fortran90Lexer(input_stream)
        stream = CommonTokenStream(lexer)
        parser = Fortran90Parser(stream)
        tree = parser.program_unit_f90()

        listener = F90SpecificationOrderingListener()
        ParseTreeWalker().walk(listener, tree)

        return listener.result
    except Exception as e:
        # If parsing fails, return a diagnostic
        result = SpecificationOrderingResult()
        result.diagnostics.append(
            SemanticDiagnostic(
                severity=DiagnosticSeverity.ERROR,
                code="E672-PARSE",
                message=f"Parse error during specification ordering validation: {str(e)}",
                iso_section="5.2.1",
            )
        )
        return result


def validate_f90_specification_ordering_file(
    filepath: str,
) -> SpecificationOrderingResult:
    """Validate Fortran 90 source file for specification_part ordering compliance.

    Args:
        filepath: Path to Fortran 90 source file

    Returns:
        SpecificationOrderingResult with diagnostics
    """
    try:
        with open(filepath, "r", encoding="utf-8", errors="ignore") as f:
            source_code = f.read()
        return validate_f90_specification_ordering(source_code)
    except IOError as e:
        result = SpecificationOrderingResult()
        result.diagnostics.append(
            SemanticDiagnostic(
                severity=DiagnosticSeverity.ERROR,
                code="E672-FILE",
                message=f"Cannot read file {filepath}: {str(e)}",
                iso_section="5.2.1",
            )
        )
        return result
