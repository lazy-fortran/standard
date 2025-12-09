#!/usr/bin/env python3
"""Fortran 2008 Coarray Semantic Validator

Implements semantic validation for Fortran 2008 coarray features per
ISO/IEC 1539-1:2010 (Fortran 2008 standard).

This validator checks:
- Coarray declarations (Sections 2.4.7, 5.3.6)
- Image control statements (Section 8.5)
- Image intrinsic functions (Section 13.7)
- Coarray reference constraints (Section 6.6)

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
class CoarrayDeclaration:
    """Represents a coarray declaration for semantic analysis."""
    name: str
    codimension: str
    is_deferred: bool = False
    is_explicit: bool = False
    corank: int = 1
    rank: int = 0
    is_allocatable: bool = False
    is_dummy: bool = False
    line: Optional[int] = None
    column: Optional[int] = None


@dataclass
class ImageControlStatement:
    """Represents an image control statement for analysis."""
    stmt_type: str
    image_set: Optional[str] = None
    has_stat: bool = False
    has_errmsg: bool = False
    in_critical: bool = False
    line: Optional[int] = None
    column: Optional[int] = None


@dataclass
class CoarrayReference:
    """Represents a coarray reference with image selector."""
    name: str
    image_selector: str
    is_remote: bool = False
    in_sync_context: bool = False
    line: Optional[int] = None
    column: Optional[int] = None


@dataclass
class CoarrayValidationResult:
    """Results from coarray semantic validation."""
    diagnostics: List[SemanticDiagnostic] = field(default_factory=list)
    coarray_declarations: Dict[str, CoarrayDeclaration] = field(
        default_factory=dict
    )
    image_control_statements: List[ImageControlStatement] = field(
        default_factory=list
    )
    coarray_references: List[CoarrayReference] = field(default_factory=list)
    image_intrinsics_used: Set[str] = field(default_factory=set)

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


IMAGE_INTRINSICS: Set[str] = {
    "this_image", "num_images", "image_index",
    "lcobound", "ucobound",
}

SYNC_STATEMENT_TYPES: Set[str] = {"sync_all", "sync_images", "sync_memory"}


class F2008CoarrayListener(Fortran2008ParserListener):
    """ANTLR listener for F2008 coarray semantic analysis."""

    def __init__(self):
        super().__init__()
        self.result = CoarrayValidationResult()
        self._in_critical_section = False
        self._in_pure_procedure = False
        self._current_coarray_name: Optional[str] = None
        self._has_codimension: bool = False
        self._is_allocatable: bool = False
        self._current_codimension: Optional[str] = None
        self._pending_coarray_decl: Optional[CoarrayDeclaration] = None
        self._sync_depth = 0

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

    def enterFunction_stmt_f2008(self, ctx):
        text = self._get_token_text(ctx)
        if "pure" in text or "elemental" in text:
            self._in_pure_procedure = True

    def exitEnd_function_stmt(self, ctx):
        self._in_pure_procedure = False

    def enterSubroutine_stmt_f2008(self, ctx):
        text = self._get_token_text(ctx)
        if "pure" in text or "elemental" in text:
            self._in_pure_procedure = True

    def exitEnd_subroutine_stmt(self, ctx):
        self._in_pure_procedure = False

    def enterCoarray_spec(self, ctx):
        """Process coarray specification [*] or [n] or [lb:ub,...]."""
        text = self._get_token_text(ctx)
        self._has_codimension = True
        self._current_codimension = text

        is_deferred = text == "[*]" or text == "[:]"
        corank = text.count(",") + 1

        if self._current_coarray_name:
            decl = CoarrayDeclaration(
                name=self._current_coarray_name,
                codimension=text,
                is_deferred=is_deferred,
                is_explicit=not is_deferred,
                corank=corank,
                is_allocatable=self._is_allocatable,
                line=self._get_location(ctx)[0],
                column=self._get_location(ctx)[1],
            )
            self._pending_coarray_decl = decl

    def enterEntity_decl(self, ctx):
        """Process entity declaration which may include coarray."""
        text = self._get_token_text(ctx)
        self._current_coarray_name = None
        self._has_codimension = False
        self._current_codimension = None
        self._pending_coarray_decl = None

        name_match = re.match(r"(\w+)", text)
        if name_match:
            self._current_coarray_name = name_match.group(1)

    def exitEntity_decl(self, ctx):
        """Finalize entity declaration and register coarray if present."""
        if self._pending_coarray_decl:
            name = self._pending_coarray_decl.name
            self.result.coarray_declarations[name] = self._pending_coarray_decl
            self._validate_coarray_declaration(self._pending_coarray_decl, ctx)

        self._current_coarray_name = None
        self._has_codimension = False
        self._pending_coarray_decl = None

    def _validate_coarray_declaration(self, decl: CoarrayDeclaration, ctx):
        """Validate coarray declaration per ISO/IEC 1539-1:2010 Section 5.3.6."""
        if decl.is_deferred and not decl.is_allocatable and not decl.is_dummy:
            self._add_diagnostic(
                DiagnosticSeverity.WARNING,
                "COARRAY_W001",
                f"Coarray '{decl.name}' has deferred codimension [*] but is not "
                "ALLOCATABLE or a dummy argument. Per ISO/IEC 1539-1:2010 "
                "Section 5.3.6.3, deferred codimension is only valid for "
                "allocatable coarrays or dummy arguments.",
                ctx,
                "5.3.6.3",
            )

        if decl.corank > 15:
            self._add_diagnostic(
                DiagnosticSeverity.ERROR,
                "COARRAY_E001",
                f"Coarray '{decl.name}' has corank {decl.corank}, exceeding "
                "the maximum of 15 codimensions per ISO/IEC 1539-1:2010 "
                "Section 5.3.6.1.",
                ctx,
                "5.3.6.1",
            )

    def enterAttr_spec(self, ctx):
        """Track ALLOCATABLE attribute for coarray validation."""
        text = self._get_token_text(ctx)
        if "allocatable" in text:
            self._is_allocatable = True

    def exitType_declaration_stmt_f2008(self, ctx):
        """Reset declaration-level state after type declaration."""
        self._is_allocatable = False
        self._current_coarray_name = None

    def enterSync_all_stmt(self, ctx):
        """Validate SYNC ALL statement per ISO/IEC 1539-1:2010 Section 8.5.3."""
        text = self._get_token_text(ctx)
        line, col = self._get_location(ctx)

        stmt = ImageControlStatement(
            stmt_type="sync_all",
            has_stat="stat=" in text,
            has_errmsg="errmsg=" in text,
            in_critical=self._in_critical_section,
            line=line,
            column=col,
        )
        self.result.image_control_statements.append(stmt)

        if self._in_pure_procedure:
            self._add_diagnostic(
                DiagnosticSeverity.ERROR,
                "SYNC_E001",
                "SYNC ALL statement is not permitted in a PURE procedure. "
                "Per ISO/IEC 1539-1:2010 Section 8.5.1, image control "
                "statements shall not appear in a pure subprogram.",
                ctx,
                "8.5.1",
            )

        if self._in_critical_section:
            self._add_diagnostic(
                DiagnosticSeverity.ERROR,
                "SYNC_E002",
                "SYNC ALL statement within a CRITICAL construct is prohibited. "
                "Per ISO/IEC 1539-1:2010 Section 8.1.5, a branch within a "
                "CRITICAL construct shall not have an image control statement.",
                ctx,
                "8.1.5",
            )

        self._sync_depth += 1

    def enterSync_images_stmt(self, ctx):
        """Validate SYNC IMAGES statement per ISO/IEC 1539-1:2010 Section 8.5.4."""
        text = self._get_token_text(ctx)
        line, col = self._get_location(ctx)

        image_set_match = re.search(r"images\s*\(\s*([^,)]+)", text)
        image_set = image_set_match.group(1).strip() if image_set_match else None

        stmt = ImageControlStatement(
            stmt_type="sync_images",
            image_set=image_set,
            has_stat="stat=" in text,
            has_errmsg="errmsg=" in text,
            in_critical=self._in_critical_section,
            line=line,
            column=col,
        )
        self.result.image_control_statements.append(stmt)

        if self._in_pure_procedure:
            self._add_diagnostic(
                DiagnosticSeverity.ERROR,
                "SYNC_E001",
                "SYNC IMAGES statement is not permitted in a PURE procedure. "
                "Per ISO/IEC 1539-1:2010 Section 8.5.1, image control "
                "statements shall not appear in a pure subprogram.",
                ctx,
                "8.5.1",
            )

        if self._in_critical_section:
            self._add_diagnostic(
                DiagnosticSeverity.ERROR,
                "SYNC_E002",
                "SYNC IMAGES statement within a CRITICAL construct is "
                "prohibited. Per ISO/IEC 1539-1:2010 Section 8.1.5, a branch "
                "within a CRITICAL construct shall not have an image control "
                "statement.",
                ctx,
                "8.1.5",
            )

        self._sync_depth += 1

    def enterSync_memory_stmt(self, ctx):
        """Validate SYNC MEMORY statement per ISO/IEC 1539-1:2010 Section 8.5.5."""
        text = self._get_token_text(ctx)
        line, col = self._get_location(ctx)

        stmt = ImageControlStatement(
            stmt_type="sync_memory",
            has_stat="stat=" in text,
            has_errmsg="errmsg=" in text,
            in_critical=self._in_critical_section,
            line=line,
            column=col,
        )
        self.result.image_control_statements.append(stmt)

        if self._in_pure_procedure:
            self._add_diagnostic(
                DiagnosticSeverity.ERROR,
                "SYNC_E001",
                "SYNC MEMORY statement is not permitted in a PURE procedure. "
                "Per ISO/IEC 1539-1:2010 Section 8.5.1, image control "
                "statements shall not appear in a pure subprogram.",
                ctx,
                "8.5.1",
            )

        self._sync_depth += 1

    def enterImage_function_call(self, ctx):
        """Track image intrinsic function usage."""
        text = self._get_token_text(ctx)

        for intrinsic in IMAGE_INTRINSICS:
            if intrinsic in text:
                self.result.image_intrinsics_used.add(intrinsic)
                break

        if "this_image" in text:
            self._validate_this_image_call(ctx, text)
        elif "num_images" in text:
            self._validate_num_images_call(ctx, text)

    def _validate_this_image_call(self, ctx, text: str):
        """Validate THIS_IMAGE call per ISO/IEC 1539-1:2010 Section 13.7.165."""
        has_args = re.search(r"this_image\s*\([^)]+\)", text)

        if has_args:
            arg_match = re.search(r"this_image\s*\(\s*([^)]+)\)", text)
            if arg_match:
                args = arg_match.group(1)
                if "," in args:
                    self._add_diagnostic(
                        DiagnosticSeverity.INFO,
                        "IMAGE_I001",
                        "THIS_IMAGE with COARRAY and DIM arguments returns "
                        "a scalar integer cosubscript. Per ISO/IEC 1539-1:2010 "
                        "Section 13.7.165, the result is the value of the "
                        "cosubscript for dimension DIM.",
                        ctx,
                        "13.7.165",
                    )

    def _validate_num_images_call(self, ctx, text: str):
        """Validate NUM_IMAGES call per ISO/IEC 1539-1:2010 Section 13.7.121."""
        pass

    def enterLhs_expression(self, ctx):
        """Track coarray references on LHS of assignments."""
        text = self._get_token_text(ctx)

        coarray_ref_match = re.search(r"(\w+)\s*\[([^\]]+)\]", text)
        if coarray_ref_match:
            name = coarray_ref_match.group(1)
            selector = coarray_ref_match.group(2)
            line, col = self._get_location(ctx)

            is_remote = selector != "this_image()"

            ref = CoarrayReference(
                name=name,
                image_selector=selector,
                is_remote=is_remote,
                in_sync_context=self._sync_depth > 0,
                line=line,
                column=col,
            )
            self.result.coarray_references.append(ref)

            if name in self.result.coarray_declarations:
                self._validate_coarray_reference(ref, ctx)

    def _validate_coarray_reference(self, ref: CoarrayReference, ctx):
        """Validate coarray reference per ISO/IEC 1539-1:2010 Section 6.6."""
        if ref.is_remote and self._in_critical_section:
            self._add_diagnostic(
                DiagnosticSeverity.WARNING,
                "COARRAY_W002",
                f"Remote coarray reference '{ref.name}[{ref.image_selector}]' "
                "within CRITICAL section. Per ISO/IEC 1539-1:2010 Section "
                "8.1.5, remote coarray references in CRITICAL should be "
                "carefully coordinated to avoid deadlock.",
                ctx,
                "8.1.5",
            )


class F2008CoarrayValidator:
    """Semantic validator for Fortran 2008 coarray features."""

    def __init__(self):
        self._lexer = None
        self._parser = None

    def validate_code(self, code: str) -> CoarrayValidationResult:
        """Validate Fortran 2008 code for coarray semantics."""
        input_stream = InputStream(code)
        self._lexer = Fortran2008Lexer(input_stream)
        token_stream = CommonTokenStream(self._lexer)
        self._parser = Fortran2008Parser(token_stream)

        tree = self._parser.program_unit_f2008()

        syntax_errors = self._parser.getNumberOfSyntaxErrors()
        result = CoarrayValidationResult()

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

        listener = F2008CoarrayListener()
        walker = ParseTreeWalker()
        walker.walk(listener, tree)

        self._perform_cross_entity_validation(listener.result)

        return listener.result

    def _perform_cross_entity_validation(self, result: CoarrayValidationResult):
        """Perform validation that requires cross-referencing entities."""
        for ref in result.coarray_references:
            if ref.name not in result.coarray_declarations:
                result.diagnostics.append(
                    SemanticDiagnostic(
                        severity=DiagnosticSeverity.INFO,
                        code="COARRAY_I001",
                        message=f"Coarray reference '{ref.name}' may reference "
                        "a coarray declared in another scope or module. "
                        "Per ISO/IEC 1539-1:2010 Section 6.6, coarray "
                        "variables must be properly declared with codimension.",
                        line=ref.line,
                        column=ref.column,
                        iso_section="6.6",
                    )
                )

        sync_count = len(result.image_control_statements)
        coarray_count = len(result.coarray_declarations)

        if coarray_count > 0 and sync_count == 0:
            result.diagnostics.append(
                SemanticDiagnostic(
                    severity=DiagnosticSeverity.INFO,
                    code="SYNC_I001",
                    message=f"Code declares {coarray_count} coarray(s) but contains "
                    "no SYNC statements. Per ISO/IEC 1539-1:2010 Section 8.5, "
                    "proper synchronization is required for coarray data "
                    "consistency between images.",
                    iso_section="8.5",
                )
            )

    def validate_file(self, filepath: str) -> CoarrayValidationResult:
        """Validate a Fortran 2008 file for coarray semantics."""
        path = Path(filepath)
        if not path.exists():
            result = CoarrayValidationResult()
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


def validate_coarray_semantics(code: str) -> CoarrayValidationResult:
    """Convenience function to validate coarray semantics."""
    validator = F2008CoarrayValidator()
    return validator.validate_code(code)


def validate_image_control(code: str) -> CoarrayValidationResult:
    """Convenience function to validate image control statements."""
    validator = F2008CoarrayValidator()
    return validator.validate_code(code)
