#!/usr/bin/env python3
"""Fortran 2008 CONTIGUOUS and Coarray ALLOCATE Semantic Validator

Implements semantic validation for Fortran 2008 CONTIGUOUS attribute and
coarray ALLOCATE features per ISO/IEC 1539-1:2010 (Fortran 2008 standard).

This validator checks:
- CONTIGUOUS attribute usage (Section 5.3.7)
- CONTIGUOUS interactions with POINTER/ALLOCATABLE (C530-C531)
- Coarray ALLOCATE statements (Section 6.7.1)
- ALLOCATE coarray codimension constraints (C628, C644)
- ALLOCATE consistency with declarations (C645-C646)

Reference: ISO/IEC 1539-1:2010 (Fortran 2008 International Standard)
"""

import re
import sys
from dataclasses import dataclass, field
from enum import Enum, auto
from pathlib import Path
from typing import Dict, List, Optional, Set, Tuple

sys.path.insert(
    0, str(Path(__file__).parent.parent / "grammars" / "generated" / "modern")
)

from antlr4 import CommonTokenStream, InputStream, ParseTreeWalker
from Fortran2008Lexer import Fortran2008Lexer
from Fortran2008Parser import Fortran2008Parser
from Fortran2008ParserListener import Fortran2008ParserListener


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
class ContiguousDeclaration:
    """Represents a CONTIGUOUS declaration for semantic analysis."""
    name: str
    has_pointer: bool = False
    has_allocatable: bool = False
    has_assumed_shape: bool = False
    rank: int = 0
    is_from_statement: bool = False
    line: Optional[int] = None
    column: Optional[int] = None


@dataclass
class AllocateStatement:
    """Represents an ALLOCATE statement for semantic analysis."""
    objects: List[str] = field(default_factory=list)
    has_type_spec: bool = False
    type_spec: Optional[str] = None
    has_coarray_spec: bool = False
    coarray_specs: Dict[str, str] = field(default_factory=dict)
    has_stat: bool = False
    has_errmsg: bool = False
    has_source: bool = False
    has_mold: bool = False
    line: Optional[int] = None
    column: Optional[int] = None


@dataclass
class VariableDeclaration:
    """Represents a variable declaration for cross-referencing."""
    name: str
    has_pointer: bool = False
    has_allocatable: bool = False
    has_contiguous: bool = False
    has_codimension: bool = False
    is_coarray: bool = False
    is_assumed_shape: bool = False
    rank: int = 0
    corank: int = 0
    line: Optional[int] = None
    column: Optional[int] = None


@dataclass
class ContiguousAllocateValidationResult:
    """Results from CONTIGUOUS and ALLOCATE semantic validation."""
    diagnostics: List[SemanticDiagnostic] = field(default_factory=list)
    contiguous_declarations: Dict[str, ContiguousDeclaration] = field(
        default_factory=dict
    )
    allocate_statements: List[AllocateStatement] = field(default_factory=list)
    variable_declarations: Dict[str, VariableDeclaration] = field(
        default_factory=dict
    )

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


class F2008ContiguousAllocateListener(Fortran2008ParserListener):
    """ANTLR listener for F2008 CONTIGUOUS and ALLOCATE semantic analysis."""

    def __init__(self):
        super().__init__()
        self.result = ContiguousAllocateValidationResult()
        self._current_var_name: Optional[str] = None
        self._current_attrs: Set[str] = set()
        self._current_array_spec: Optional[str] = None
        self._current_coarray_spec: Optional[str] = None
        self._pending_var_decl: Optional[VariableDeclaration] = None
        self._in_allocate_stmt: bool = False
        self._current_allocate: Optional[AllocateStatement] = None
        self._in_contiguous_stmt: bool = False
        self._contiguous_stmt_names: List[str] = []

    def _get_token_text(self, ctx) -> str:
        if ctx is None:
            return ""
        return ctx.getText().lower()

    def _get_location(self, ctx) -> Tuple[Optional[int], Optional[int]]:
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

    def enterAttr_spec(self, ctx):
        """Track attribute specifications for declarations."""
        text = self._get_token_text(ctx)
        if "contiguous" in text:
            self._current_attrs.add("contiguous")
        if "pointer" in text:
            self._current_attrs.add("pointer")
        if "allocatable" in text:
            self._current_attrs.add("allocatable")

    def enterType_declaration_stmt_f2008(self, ctx):
        """Reset state for new type declaration."""
        self._current_attrs = set()
        self._current_var_name = None
        self._current_array_spec = None
        self._current_coarray_spec = None

    def exitType_declaration_stmt_f2008(self, ctx):
        """Finalize type declaration and validate CONTIGUOUS constraints."""
        self._current_attrs = set()

    def enterEntity_decl(self, ctx):
        """Process entity declaration."""
        text = self._get_token_text(ctx)
        self._current_var_name = None
        self._current_array_spec = None
        self._current_coarray_spec = None

        name_match = re.match(r"(\w+)", text)
        if name_match:
            self._current_var_name = name_match.group(1)

        array_match = re.search(r"\(([^)]+)\)", text)
        if array_match:
            self._current_array_spec = array_match.group(1)

        coarray_match = re.search(r"\[([^\]]+)\]", text)
        if coarray_match:
            self._current_coarray_spec = coarray_match.group(1)

    def exitEntity_decl(self, ctx):
        """Finalize entity declaration and create variable record."""
        if not self._current_var_name:
            return

        is_assumed_shape = False
        rank = 0
        if self._current_array_spec:
            dims = self._current_array_spec.split(",")
            rank = len(dims)
            is_assumed_shape = any(":" in d and d.strip() == ":" for d in dims)

        corank = 0
        if self._current_coarray_spec:
            corank = self._current_coarray_spec.count(",") + 1

        has_contiguous = "contiguous" in self._current_attrs
        has_pointer = "pointer" in self._current_attrs
        has_allocatable = "allocatable" in self._current_attrs

        var_decl = VariableDeclaration(
            name=self._current_var_name,
            has_pointer=has_pointer,
            has_allocatable=has_allocatable,
            has_contiguous=has_contiguous,
            has_codimension=self._current_coarray_spec is not None,
            is_coarray=self._current_coarray_spec is not None,
            is_assumed_shape=is_assumed_shape,
            rank=rank,
            corank=corank,
            line=self._get_location(ctx)[0],
            column=self._get_location(ctx)[1],
        )
        self.result.variable_declarations[self._current_var_name] = var_decl

        if has_contiguous:
            contiguous_decl = ContiguousDeclaration(
                name=self._current_var_name,
                has_pointer=has_pointer,
                has_allocatable=has_allocatable,
                has_assumed_shape=is_assumed_shape,
                rank=rank,
                is_from_statement=False,
                line=self._get_location(ctx)[0],
                column=self._get_location(ctx)[1],
            )
            self.result.contiguous_declarations[self._current_var_name] = contiguous_decl
            self._validate_contiguous_declaration(contiguous_decl, ctx)

        self._current_var_name = None
        self._current_array_spec = None
        self._current_coarray_spec = None

    def _validate_contiguous_declaration(
        self, decl: ContiguousDeclaration, ctx
    ):
        """Validate CONTIGUOUS declaration per ISO/IEC 1539-1:2010 Section 5.3.7."""
        if not decl.has_pointer and not decl.has_assumed_shape:
            self._add_diagnostic(
                DiagnosticSeverity.WARNING,
                "CONTIG_W001",
                f"CONTIGUOUS attribute on '{decl.name}' may be unnecessary. "
                "Per ISO/IEC 1539-1:2010 Section 5.3.7 C530, CONTIGUOUS is "
                "primarily meaningful for assumed-shape dummy arguments or "
                "array pointers.",
                ctx,
                "5.3.7",
            )

        if decl.rank == 0:
            self._add_diagnostic(
                DiagnosticSeverity.ERROR,
                "CONTIG_E001",
                f"CONTIGUOUS attribute on scalar '{decl.name}' is invalid. "
                "Per ISO/IEC 1539-1:2010 Section 5.3.7 C530, CONTIGUOUS "
                "requires an array with rank >= 1.",
                ctx,
                "5.3.7",
            )

    def enterContiguous_stmt(self, ctx):
        """Process CONTIGUOUS statement."""
        self._in_contiguous_stmt = True
        self._contiguous_stmt_names = []

    def exitContiguous_stmt(self, ctx):
        """Finalize CONTIGUOUS statement processing."""
        text = self._get_token_text(ctx)

        name_list_match = re.search(r"::\s*(.+)", text)
        if name_list_match:
            names_str = name_list_match.group(1).strip()
            names_str = re.sub(r"\s+", "", names_str)
            names = [n.strip() for n in names_str.split(",") if n.strip()]

            for name in names:
                clean_name = re.match(r"(\w+)", name)
                if clean_name:
                    var_name = clean_name.group(1)
                    contiguous_decl = ContiguousDeclaration(
                        name=var_name,
                        is_from_statement=True,
                        line=self._get_location(ctx)[0],
                        column=self._get_location(ctx)[1],
                    )
                    self.result.contiguous_declarations[var_name] = contiguous_decl

                    if var_name in self.result.variable_declarations:
                        var_decl = self.result.variable_declarations[var_name]
                        contiguous_decl.has_pointer = var_decl.has_pointer
                        contiguous_decl.has_allocatable = var_decl.has_allocatable
                        contiguous_decl.has_assumed_shape = var_decl.is_assumed_shape
                        contiguous_decl.rank = var_decl.rank
                        var_decl.has_contiguous = True

        self._in_contiguous_stmt = False
        self._contiguous_stmt_names = []

    def enterAllocate_stmt_f2008(self, ctx):
        """Process F2008 ALLOCATE statement."""
        self._in_allocate_stmt = True
        text = self._get_token_text(ctx)
        line, col = self._get_location(ctx)

        self._current_allocate = AllocateStatement(
            has_type_spec="::" in text,
            has_stat="stat=" in text,
            has_errmsg="errmsg=" in text,
            has_source="source=" in text,
            has_mold="mold=" in text,
            line=line,
            column=col,
        )

        if "::" in text:
            type_match = re.search(r"\(\s*(\w+)\s*::", text)
            if type_match:
                self._current_allocate.type_spec = type_match.group(1)

    def exitAllocate_stmt_f2008(self, ctx):
        """Finalize ALLOCATE statement and validate."""
        if self._current_allocate:
            self.result.allocate_statements.append(self._current_allocate)
            self._validate_allocate_statement(self._current_allocate, ctx)
        self._in_allocate_stmt = False
        self._current_allocate = None

    def enterAllocation_f2008(self, ctx):
        """Process individual allocation within ALLOCATE statement."""
        if not self._current_allocate:
            return

        text = self._get_token_text(ctx)

        name_match = re.match(r"(\w+)", text)
        if name_match:
            obj_name = name_match.group(1)
            self._current_allocate.objects.append(obj_name)

            coarray_match = re.search(r"\[([^\]]+)\]", text)
            if coarray_match:
                self._current_allocate.has_coarray_spec = True
                self._current_allocate.coarray_specs[obj_name] = coarray_match.group(1)

    def _validate_allocate_statement(self, alloc: AllocateStatement, ctx):
        """Validate ALLOCATE statement per ISO/IEC 1539-1:2010 Section 6.7.1."""
        for obj_name in alloc.objects:
            if obj_name in self.result.variable_declarations:
                var_decl = self.result.variable_declarations[obj_name]

                if not var_decl.has_allocatable and not var_decl.has_pointer:
                    self._add_diagnostic(
                        DiagnosticSeverity.ERROR,
                        "ALLOC_E001",
                        f"ALLOCATE object '{obj_name}' must have ALLOCATABLE "
                        "or POINTER attribute. Per ISO/IEC 1539-1:2010 "
                        "Section 6.7.1.1 C628, each allocate-object shall be "
                        "a data pointer or allocatable variable.",
                        ctx,
                        "6.7.1.1",
                    )

                if var_decl.is_coarray:
                    if obj_name in alloc.coarray_specs:
                        alloc_cospec = alloc.coarray_specs[obj_name]
                        self._validate_coarray_allocate(
                            obj_name, var_decl, alloc_cospec, ctx
                        )
                    elif var_decl.corank > 0 and var_decl.has_allocatable:
                        self._add_diagnostic(
                            DiagnosticSeverity.WARNING,
                            "ALLOC_W001",
                            f"Coarray '{obj_name}' in ALLOCATE may require "
                            "coarray codimension specification [*]. Per "
                            "ISO/IEC 1539-1:2010 Section 6.7.1.2 C644, "
                            "an allocatable coarray shall have deferred "
                            "cobounds in its declaration.",
                            ctx,
                            "6.7.1.2",
                        )

        if alloc.has_source and alloc.has_mold:
            self._add_diagnostic(
                DiagnosticSeverity.ERROR,
                "ALLOC_E002",
                "ALLOCATE statement cannot have both SOURCE= and MOLD= "
                "specifiers. Per ISO/IEC 1539-1:2010 Section 6.7.1.1 C636, "
                "at most one of SOURCE= or MOLD= may appear.",
                ctx,
                "6.7.1.1",
            )

        if len(alloc.objects) > 1 and (alloc.has_source or alloc.has_mold):
            if alloc.has_coarray_spec:
                self._add_diagnostic(
                    DiagnosticSeverity.INFO,
                    "ALLOC_I001",
                    "ALLOCATE with SOURCE=/MOLD= and multiple coarray objects "
                    "requires careful synchronization. Per ISO/IEC 1539-1:2010 "
                    "Section 6.7.1.2, coarray allocation is an image control "
                    "statement that synchronizes all images.",
                    ctx,
                    "6.7.1.2",
                )

    def _validate_coarray_allocate(
        self,
        obj_name: str,
        var_decl: VariableDeclaration,
        alloc_cospec: str,
        ctx
    ):
        """Validate coarray ALLOCATE codimension per ISO/IEC 1539-1:2010."""
        if "*" not in alloc_cospec:
            self._add_diagnostic(
                DiagnosticSeverity.WARNING,
                "ALLOC_W002",
                f"Coarray '{obj_name}' ALLOCATE codimension '{alloc_cospec}' "
                "should typically end with '*' to indicate variable upper "
                "cobound. Per ISO/IEC 1539-1:2010 Section 6.7.1.2, the "
                "upper bound of the last codimension is determined at "
                "execution time.",
                ctx,
                "6.7.1.2",
            )

        alloc_corank = alloc_cospec.count(",") + 1
        if var_decl.corank > 0 and alloc_corank != var_decl.corank:
            self._add_diagnostic(
                DiagnosticSeverity.ERROR,
                "ALLOC_E003",
                f"Coarray '{obj_name}' ALLOCATE corank ({alloc_corank}) does "
                f"not match declaration corank ({var_decl.corank}). Per "
                "ISO/IEC 1539-1:2010 Section 6.7.1.2 C645, the number of "
                "cosubscripts shall equal the corank of the allocate-object.",
                ctx,
                "6.7.1.2",
            )


class F2008ContiguousAllocateValidator:
    """Semantic validator for Fortran 2008 CONTIGUOUS and ALLOCATE features."""

    def __init__(self):
        self._lexer = None
        self._parser = None

    def validate_code(self, code: str) -> ContiguousAllocateValidationResult:
        """Validate Fortran 2008 code for CONTIGUOUS and ALLOCATE semantics."""
        input_stream = InputStream(code)
        self._lexer = Fortran2008Lexer(input_stream)
        token_stream = CommonTokenStream(self._lexer)
        self._parser = Fortran2008Parser(token_stream)

        tree = self._parser.program_unit_f2008()

        syntax_errors = self._parser.getNumberOfSyntaxErrors()
        result = ContiguousAllocateValidationResult()

        if syntax_errors > 0:
            result.diagnostics.append(
                SemanticDiagnostic(
                    severity=DiagnosticSeverity.ERROR,
                    code="SYNTAX_E001",
                    message=f"Code contains {syntax_errors} syntax error(s). "
                    "Fix syntax errors before semantic validation.",
                )
            )
            return result

        listener = F2008ContiguousAllocateListener()
        walker = ParseTreeWalker()
        walker.walk(listener, tree)

        self._perform_cross_entity_validation(listener.result)

        return listener.result

    def _perform_cross_entity_validation(
        self, result: ContiguousAllocateValidationResult
    ):
        """Perform validation that requires cross-referencing entities."""
        for name, contiguous_decl in result.contiguous_declarations.items():
            if contiguous_decl.is_from_statement:
                if name not in result.variable_declarations:
                    result.diagnostics.append(
                        SemanticDiagnostic(
                            severity=DiagnosticSeverity.INFO,
                            code="CONTIG_I001",
                            message=f"CONTIGUOUS statement references '{name}' "
                            "which may be declared in another scope or module. "
                            "Per ISO/IEC 1539-1:2010 Section 5.3.7, the "
                            "object must be an array pointer or assumed-shape "
                            "dummy argument.",
                            line=contiguous_decl.line,
                            column=contiguous_decl.column,
                            iso_section="5.3.7",
                        )
                    )
                else:
                    var_decl = result.variable_declarations[name]
                    if var_decl.rank == 0:
                        result.diagnostics.append(
                            SemanticDiagnostic(
                                severity=DiagnosticSeverity.ERROR,
                                code="CONTIG_E002",
                                message=f"CONTIGUOUS statement applied to scalar "
                                f"'{name}' is invalid. Per ISO/IEC 1539-1:2010 "
                                "Section 5.3.7 C530, CONTIGUOUS requires an "
                                "array with rank >= 1.",
                                line=contiguous_decl.line,
                                column=contiguous_decl.column,
                                iso_section="5.3.7",
                            )
                        )

        for alloc in result.allocate_statements:
            for obj_name in alloc.objects:
                if obj_name not in result.variable_declarations:
                    result.diagnostics.append(
                        SemanticDiagnostic(
                            severity=DiagnosticSeverity.INFO,
                            code="ALLOC_I002",
                            message=f"ALLOCATE object '{obj_name}' may be declared "
                            "in another scope or module. Per ISO/IEC 1539-1:2010 "
                            "Section 6.7.1.1 C628, it must have ALLOCATABLE or "
                            "POINTER attribute.",
                            line=alloc.line,
                            column=alloc.column,
                            iso_section="6.7.1.1",
                        )
                    )

    def validate_file(self, filepath: str) -> ContiguousAllocateValidationResult:
        """Validate a Fortran 2008 file for CONTIGUOUS and ALLOCATE semantics."""
        path = Path(filepath)
        if not path.exists():
            result = ContiguousAllocateValidationResult()
            result.diagnostics.append(
                SemanticDiagnostic(
                    severity=DiagnosticSeverity.ERROR,
                    code="FILE_E001",
                    message=f"File not found: {filepath}",
                )
            )
            return result

        code = path.read_text()
        return self.validate_code(code)


def validate_contiguous_semantics(code: str) -> ContiguousAllocateValidationResult:
    """Convenience function to validate CONTIGUOUS semantics."""
    validator = F2008ContiguousAllocateValidator()
    return validator.validate_code(code)


def validate_allocate_semantics(code: str) -> ContiguousAllocateValidationResult:
    """Convenience function to validate ALLOCATE semantics."""
    validator = F2008ContiguousAllocateValidator()
    return validator.validate_code(code)
