#!/usr/bin/env python3
"""Fortran 2018 DO CONCURRENT with Locality Semantic Validator

Implements semantic validation for Fortran 2018 DO CONCURRENT construct with
locality specifiers per ISO/IEC 1539-1:2018 (Fortran 2018 standard).

This validator checks:
- DO CONCURRENT with locality specifiers (Section 11.1.7.2)
- LOCAL, LOCAL_INIT, SHARED, DEFAULT(NONE) semantics (R1130)
- Iteration independence with locality constraints (Section 11.1.7.5)
- Variable declaration and usage requirements
- Interactions with coarrays and teams

Reference: ISO/IEC 1539-1:2018 (Fortran 2018 International Standard)
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
from Fortran2018Lexer import Fortran2018Lexer
from Fortran2018Parser import Fortran2018Parser
from Fortran2018ParserListener import Fortran2018ParserListener

from fortran_intrinsics import FORTRAN_KEYWORDS_AND_INTRINSICS


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
class LocalitySpec:
    """Represents a locality specifier in DO CONCURRENT."""
    spec_type: str
    variables: List[str] = field(default_factory=list)
    line: Optional[int] = None
    column: Optional[int] = None


@dataclass
class DoConcurrentLocalityConstruct:
    """Represents a DO CONCURRENT construct with locality for semantic analysis.

    Per ISO/IEC 1539-1:2018 Section 11.1.7.2, DO CONCURRENT may include
    locality specifiers that define the scope and initialization of variables.
    """
    index_variables: List[str] = field(default_factory=list)
    has_mask: bool = False
    mask_expr: Optional[str] = None
    local_variables: Set[str] = field(default_factory=set)
    local_init_variables: Set[str] = field(default_factory=set)
    shared_variables: Set[str] = field(default_factory=set)
    has_default_none: bool = False
    locality_specs: List[LocalitySpec] = field(default_factory=list)
    assigned_variables: Set[str] = field(default_factory=set)
    referenced_variables: Set[str] = field(default_factory=set)
    contains_io: bool = False
    contains_stop: bool = False
    contains_sync: bool = False
    contains_allocate: bool = False
    contains_deallocate: bool = False
    contains_coarray_ref: bool = False
    contains_procedure_call: bool = False
    called_procedures: Set[str] = field(default_factory=set)
    depth: int = 0
    line: Optional[int] = None
    column: Optional[int] = None


@dataclass
class DoConcurrentLocalityValidationResult:
    """Results from DO CONCURRENT with locality semantic validation."""
    diagnostics: List[SemanticDiagnostic] = field(default_factory=list)
    do_concurrent_constructs: List[DoConcurrentLocalityConstruct] = field(
        default_factory=list
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


class F2018DoConcurrentLocalityListener(Fortran2018ParserListener):
    """ANTLR listener for F2018 DO CONCURRENT with locality semantic analysis."""

    def __init__(self):
        super().__init__()
        self.result = DoConcurrentLocalityValidationResult()
        self._do_concurrent_stack: List[DoConcurrentLocalityConstruct] = []
        self._in_do_concurrent = False
        self._current_do_concurrent: Optional[DoConcurrentLocalityConstruct] = None
        self._pure_context = False
        self._impure_allowed = True
        self._declared_variables: Set[str] = set()
        self._in_locality_spec = False

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

    def enterType_declaration_stmt(self, ctx):
        """Track variable declarations for locality validation."""
        text = self._get_token_text(ctx)
        decl_match = re.search(r"::\s*(.+)$", text)
        if decl_match:
            decl_part = decl_match.group(1)
            var_names = re.findall(r"(\w+)", decl_part)
            for var in var_names:
                if var not in ["dimension", "allocatable", "intent", "inout", "in",
                               "out", "optional", "save", "target", "pointer"]:
                    self._declared_variables.add(var)

    def enterDo_concurrent_construct_f2018(self, ctx):
        """Enter a DO CONCURRENT construct with F2018 locality support."""
        line, col = self._get_location(ctx)
        construct = DoConcurrentLocalityConstruct(
            depth=len(self._do_concurrent_stack),
            line=line,
            column=col,
        )
        self._do_concurrent_stack.append(construct)
        self._in_do_concurrent = True
        self._current_do_concurrent = construct

    def exitDo_concurrent_construct_f2018(self, ctx):
        """Exit a DO CONCURRENT construct and perform validation."""
        if self._do_concurrent_stack:
            construct = self._do_concurrent_stack.pop()
            self.result.do_concurrent_constructs.append(construct)
            self._validate_do_concurrent_locality(construct, ctx)
            if self._do_concurrent_stack:
                self._current_do_concurrent = self._do_concurrent_stack[-1]
            else:
                self._current_do_concurrent = None
                self._in_do_concurrent = False

    def enterDo_concurrent_stmt_f2018(self, ctx):
        """Process DO CONCURRENT statement header with locality specifiers."""
        text = self._get_token_text(ctx)
        if self._current_do_concurrent is None:
            return

        index_vars = self._extract_index_variables(text)
        self._current_do_concurrent.index_variables = index_vars

        paren_match = re.search(r"\(([^)]+)\)", text)
        if paren_match:
            header_content = paren_match.group(1)
            if "," in header_content:
                parts = header_content.split(",")
                for part in parts[1:]:
                    part = part.strip()
                    if not re.match(r"\w+\s*=", part):
                        self._current_do_concurrent.has_mask = True
                        break

    def enterConcurrent_locality(self, ctx):
        """Process locality specifiers per ISO/IEC 1539-1:2018 R1130."""
        text = self._get_token_text(ctx)
        if self._current_do_concurrent is None:
            return

        line, col = self._get_location(ctx)

        if text.startswith("local_init"):
            var_match = re.search(r"local_init\s*\(([^)]+)\)", text)
            if var_match:
                vars_text = var_match.group(1)
                variables = [v.strip() for v in vars_text.split(",")]
                spec = LocalitySpec(
                    spec_type="local_init",
                    variables=variables,
                    line=line,
                    column=col,
                )
                self._current_do_concurrent.locality_specs.append(spec)
                self._current_do_concurrent.local_init_variables.update(variables)
        elif text.startswith("local"):
            var_match = re.search(r"local\s*\(([^)]+)\)", text)
            if var_match:
                vars_text = var_match.group(1)
                variables = [v.strip() for v in vars_text.split(",")]
                spec = LocalitySpec(
                    spec_type="local",
                    variables=variables,
                    line=line,
                    column=col,
                )
                self._current_do_concurrent.locality_specs.append(spec)
                self._current_do_concurrent.local_variables.update(variables)
        elif text.startswith("shared"):
            var_match = re.search(r"shared\s*\(([^)]+)\)", text)
            if var_match:
                vars_text = var_match.group(1)
                variables = [v.strip() for v in vars_text.split(",")]
                spec = LocalitySpec(
                    spec_type="shared",
                    variables=variables,
                    line=line,
                    column=col,
                )
                self._current_do_concurrent.locality_specs.append(spec)
                self._current_do_concurrent.shared_variables.update(variables)
        elif "default" in text and "none" in text:
            spec = LocalitySpec(
                spec_type="default_none",
                variables=[],
                line=line,
                column=col,
            )
            self._current_do_concurrent.locality_specs.append(spec)
            self._current_do_concurrent.has_default_none = True

    def _extract_index_variables(self, text: str) -> List[str]:
        """Extract index variable names from DO CONCURRENT header."""
        vars_found = []
        pattern = r"(\w+)\s*=\s*[^:,]+:[^:,]+"
        for match in re.finditer(pattern, text):
            var_name = match.group(1)
            if var_name not in ["concurrent", "do"]:
                vars_found.append(var_name)
        return vars_found

    def enterAssignment_stmt(self, ctx):
        """Track variable assignments and references within DO CONCURRENT."""
        if not self._in_do_concurrent or self._current_do_concurrent is None:
            return

        text = self._get_token_text(ctx)
        lhs_match = re.match(r"(\w+)", text)
        if lhs_match:
            var_name = lhs_match.group(1)
            self._current_do_concurrent.assigned_variables.add(var_name)

        equals_pos = text.find("=")
        if equals_pos != -1:
            rhs_text = text[equals_pos + 1:]
            rhs_vars = self._extract_referenced_variables(rhs_text)
            self._current_do_concurrent.referenced_variables.update(rhs_vars)

    def _extract_referenced_variables(self, text: str) -> Set[str]:
        """Extract variable names referenced in an expression.

        Uses the centralized FORTRAN_KEYWORDS_AND_INTRINSICS set from
        fortran_intrinsics module to filter out intrinsic procedure names,
        reducing false positives in LOC_I001 diagnostics.
        """
        identifiers = set(re.findall(r"\b([a-z_]\w*)\b", text))
        return identifiers - FORTRAN_KEYWORDS_AND_INTRINSICS

    def enterCall_stmt(self, ctx):
        """Track procedure calls within DO CONCURRENT."""
        if not self._in_do_concurrent or self._current_do_concurrent is None:
            return

        text = self._get_token_text(ctx)
        self._current_do_concurrent.contains_procedure_call = True

        call_match = re.search(r"call\s+(\w+)", text)
        if call_match:
            proc_name = call_match.group(1)
            self._current_do_concurrent.called_procedures.add(proc_name)

    def enterPrint_stmt(self, ctx):
        """Track PRINT statements within DO CONCURRENT."""
        if not self._in_do_concurrent or self._current_do_concurrent is None:
            return
        self._current_do_concurrent.contains_io = True
        self._add_diagnostic(
            DiagnosticSeverity.WARNING,
            "LOC_W001",
            "PRINT statement within DO CONCURRENT may cause nondeterministic "
            "output ordering. Per ISO/IEC 1539-1:2018 Section 11.1.7.5, "
            "I/O operations are not recommended in DO CONCURRENT.",
            ctx,
            "11.1.7.5",
        )

    def enterStop_stmt(self, ctx):
        """Track STOP statements within DO CONCURRENT."""
        if not self._in_do_concurrent or self._current_do_concurrent is None:
            return
        self._current_do_concurrent.contains_stop = True
        self._add_diagnostic(
            DiagnosticSeverity.ERROR,
            "LOC_E001",
            "STOP statement is prohibited within DO CONCURRENT. "
            "Per ISO/IEC 1539-1:2018 Section 11.1.7.5, a STOP or ERROR STOP "
            "statement shall not appear within a DO CONCURRENT construct.",
            ctx,
            "11.1.7.5",
        )

    def enterStop_stmt_f2018(self, ctx):
        """Track F2018 STOP statements within DO CONCURRENT."""
        if not self._in_do_concurrent or self._current_do_concurrent is None:
            return
        self._current_do_concurrent.contains_stop = True
        self._add_diagnostic(
            DiagnosticSeverity.ERROR,
            "LOC_E001",
            "STOP statement is prohibited within DO CONCURRENT. "
            "Per ISO/IEC 1539-1:2018 Section 11.1.7.5, a STOP or ERROR STOP "
            "statement shall not appear within a DO CONCURRENT construct.",
            ctx,
            "11.1.7.5",
        )

    def enterError_stop_stmt(self, ctx):
        """Track ERROR STOP statements within DO CONCURRENT."""
        if not self._in_do_concurrent or self._current_do_concurrent is None:
            return
        self._current_do_concurrent.contains_stop = True
        self._add_diagnostic(
            DiagnosticSeverity.ERROR,
            "LOC_E002",
            "ERROR STOP statement is prohibited within DO CONCURRENT. "
            "Per ISO/IEC 1539-1:2018 Section 11.1.7.5, a STOP or ERROR STOP "
            "statement shall not appear within a DO CONCURRENT construct.",
            ctx,
            "11.1.7.5",
        )

    def enterSync_all_stmt(self, ctx):
        """Track SYNC ALL statements within DO CONCURRENT."""
        if not self._in_do_concurrent or self._current_do_concurrent is None:
            return
        self._current_do_concurrent.contains_sync = True
        self._add_diagnostic(
            DiagnosticSeverity.ERROR,
            "LOC_E003",
            "Image control statement SYNC ALL is prohibited within "
            "DO CONCURRENT. Per ISO/IEC 1539-1:2018 Section 11.1.7.5, "
            "an image control statement shall not appear within a "
            "DO CONCURRENT construct.",
            ctx,
            "11.1.7.5",
        )

    def enterSync_images_stmt(self, ctx):
        """Track SYNC IMAGES statements within DO CONCURRENT."""
        if not self._in_do_concurrent or self._current_do_concurrent is None:
            return
        self._current_do_concurrent.contains_sync = True
        self._add_diagnostic(
            DiagnosticSeverity.ERROR,
            "LOC_E004",
            "Image control statement SYNC IMAGES is prohibited within "
            "DO CONCURRENT. Per ISO/IEC 1539-1:2018 Section 11.1.7.5, "
            "an image control statement shall not appear within a "
            "DO CONCURRENT construct.",
            ctx,
            "11.1.7.5",
        )

    def enterSync_memory_stmt(self, ctx):
        """Track SYNC MEMORY statements within DO CONCURRENT."""
        if not self._in_do_concurrent or self._current_do_concurrent is None:
            return
        self._current_do_concurrent.contains_sync = True
        self._add_diagnostic(
            DiagnosticSeverity.ERROR,
            "LOC_E005",
            "Image control statement SYNC MEMORY is prohibited within "
            "DO CONCURRENT. Per ISO/IEC 1539-1:2018 Section 11.1.7.5, "
            "an image control statement shall not appear within a "
            "DO CONCURRENT construct.",
            ctx,
            "11.1.7.5",
        )

    def enterAllocate_stmt(self, ctx):
        """Track ALLOCATE statements within DO CONCURRENT."""
        if not self._in_do_concurrent or self._current_do_concurrent is None:
            return
        self._current_do_concurrent.contains_allocate = True
        text = self._get_token_text(ctx)
        if "[" in text:
            self._current_do_concurrent.contains_coarray_ref = True
            self._add_diagnostic(
                DiagnosticSeverity.ERROR,
                "LOC_E006",
                "ALLOCATE with coarray is prohibited within DO CONCURRENT. "
                "Per ISO/IEC 1539-1:2018 Section 11.1.7.5, allocation of "
                "coarrays shall not appear within a DO CONCURRENT construct.",
                ctx,
                "11.1.7.5",
            )

    def _validate_do_concurrent_locality(
        self, construct: DoConcurrentLocalityConstruct, ctx
    ):
        """Validate a completed DO CONCURRENT construct with locality.

        Per ISO/IEC 1539-1:2018 Section 11.1.7.2-11.1.7.5, validates:
        - Locality specifier syntax and semantics
        - Variable declarations for locality specs
        - Index variable restrictions
        - DEFAULT(NONE) completeness
        - Duplicate locality specifications
        """
        self._validate_index_variable_locality(construct, ctx)
        self._validate_duplicate_locality(construct, ctx)
        self._validate_default_none_completeness(construct, ctx)
        self._validate_local_variable_usage(construct, ctx)
        self._validate_shared_variable_usage(construct, ctx)
        self._validate_iteration_independence(construct, ctx)

    def _validate_index_variable_locality(
        self, construct: DoConcurrentLocalityConstruct, ctx
    ):
        """Check that index variables are not in locality specs.

        Per ISO/IEC 1539-1:2018 Section 11.1.7.4, the index-name of a
        concurrent-control shall not appear in a locality-spec.
        """
        for idx_var in construct.index_variables:
            all_locality_vars = (
                construct.local_variables |
                construct.local_init_variables |
                construct.shared_variables
            )
            if idx_var in all_locality_vars:
                self._add_diagnostic(
                    DiagnosticSeverity.ERROR,
                    "LOC_E007",
                    f"Index variable '{idx_var}' shall not appear in a "
                    "locality specifier. Per ISO/IEC 1539-1:2018 Section "
                    "11.1.7.4, the index-name of a concurrent-control is "
                    "implicitly LOCAL and cannot be explicitly specified.",
                    ctx,
                    "11.1.7.4",
                )

    def _validate_duplicate_locality(
        self, construct: DoConcurrentLocalityConstruct, ctx
    ):
        """Check for duplicate variables in locality specs.

        Per ISO/IEC 1539-1:2018 Section 11.1.7.4, a variable-name shall
        not appear in more than one locality-spec.
        """
        all_vars: Dict[str, str] = {}
        for spec in construct.locality_specs:
            for var in spec.variables:
                if var in all_vars:
                    self._add_diagnostic(
                        DiagnosticSeverity.ERROR,
                        "LOC_E008",
                        f"Variable '{var}' appears in multiple locality "
                        f"specifiers ({all_vars[var]} and {spec.spec_type}). "
                        "Per ISO/IEC 1539-1:2018 Section 11.1.7.4, a "
                        "variable-name shall not appear in more than one "
                        "locality-spec.",
                        ctx,
                        "11.1.7.4",
                    )
                else:
                    all_vars[var] = spec.spec_type

    def _validate_default_none_completeness(
        self, construct: DoConcurrentLocalityConstruct, ctx
    ):
        """Check DEFAULT(NONE) completeness.

        Per ISO/IEC 1539-1:2018 Section 11.1.7.5, if DEFAULT(NONE) appears,
        each variable referenced in the DO CONCURRENT block must have its
        locality explicitly specified.
        """
        if not construct.has_default_none:
            return

        all_locality_vars = (
            construct.local_variables |
            construct.local_init_variables |
            construct.shared_variables |
            set(construct.index_variables)
        )

        all_referenced = construct.referenced_variables | construct.assigned_variables

        for var in all_referenced:
            if var not in all_locality_vars:
                self._add_diagnostic(
                    DiagnosticSeverity.ERROR,
                    "LOC_E009",
                    f"Variable '{var}' used in DO CONCURRENT with "
                    "DEFAULT(NONE) but has no locality specifier. Per "
                    "ISO/IEC 1539-1:2018 Section 11.1.7.5, when DEFAULT(NONE) "
                    "is specified, each variable used in the loop shall have "
                    "an explicit locality specification.",
                    ctx,
                    "11.1.7.5",
                )

    def _validate_local_variable_usage(
        self, construct: DoConcurrentLocalityConstruct, ctx
    ):
        """Validate LOCAL variable semantics.

        Per ISO/IEC 1539-1:2018 Section 11.1.7.4, a LOCAL variable is a
        construct entity with the same type, type parameters, and rank as
        the variable with that name outside the construct, but it is
        undefined at the beginning of each iteration.
        """
        for var in construct.local_variables:
            if var in construct.referenced_variables:
                if var not in construct.assigned_variables:
                    self._add_diagnostic(
                        DiagnosticSeverity.WARNING,
                        "LOC_W002",
                        f"LOCAL variable '{var}' is referenced but may be "
                        "undefined. Per ISO/IEC 1539-1:2018 Section 11.1.7.4, "
                        "LOCAL variables are undefined at the start of each "
                        "iteration. Ensure the variable is assigned before use.",
                        ctx,
                        "11.1.7.4",
                    )

    def _validate_shared_variable_usage(
        self, construct: DoConcurrentLocalityConstruct, ctx
    ):
        """Validate SHARED variable semantics.

        Per ISO/IEC 1539-1:2018 Section 11.1.7.5, a SHARED variable is
        shared across all iterations. For iteration independence, SHARED
        variables should generally not be assigned within the loop unless
        all iterations assign the same value or there is synchronization.
        """
        for var in construct.shared_variables:
            if var in construct.assigned_variables:
                self._add_diagnostic(
                    DiagnosticSeverity.WARNING,
                    "LOC_W003",
                    f"SHARED variable '{var}' is assigned within DO "
                    "CONCURRENT. Per ISO/IEC 1539-1:2018 Section 11.1.7.5, "
                    "assigning to a SHARED variable may violate iteration "
                    "independence unless all iterations assign the same value.",
                    ctx,
                    "11.1.7.5",
                )

    def _validate_iteration_independence(
        self, construct: DoConcurrentLocalityConstruct, ctx
    ):
        """Check for potential iteration independence violations.

        Per ISO/IEC 1539-1:2018 Section 11.1.7.5, iterations of DO CONCURRENT
        must be able to execute in any order or concurrently.
        """
        unspecified_assigned = (
            construct.assigned_variables -
            construct.local_variables -
            construct.local_init_variables -
            construct.shared_variables -
            set(construct.index_variables)
        )

        for var in unspecified_assigned:
            if var in construct.referenced_variables:
                if not construct.has_default_none:
                    self._add_diagnostic(
                        DiagnosticSeverity.INFO,
                        "LOC_I001",
                        f"Variable '{var}' is both assigned and referenced "
                        "within DO CONCURRENT without explicit locality. "
                        "Per ISO/IEC 1539-1:2018 Section 11.1.7.5, consider "
                        "adding a locality specifier (LOCAL, LOCAL_INIT, or "
                        "SHARED) to clarify iteration independence.",
                        ctx,
                        "11.1.7.5",
                    )


class F2018DoConcurrentLocalityValidator:
    """Semantic validator for Fortran 2018 DO CONCURRENT with locality."""

    def __init__(self):
        self._lexer = None
        self._parser = None

    def validate_code(self, code: str) -> DoConcurrentLocalityValidationResult:
        """Validate Fortran 2018 code for DO CONCURRENT with locality."""
        input_stream = InputStream(code)
        self._lexer = Fortran2018Lexer(input_stream)
        token_stream = CommonTokenStream(self._lexer)
        self._parser = Fortran2018Parser(token_stream)

        tree = self._parser.program_unit_f2018()

        syntax_errors = self._parser.getNumberOfSyntaxErrors()
        result = DoConcurrentLocalityValidationResult()

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

        listener = F2018DoConcurrentLocalityListener()
        walker = ParseTreeWalker()
        walker.walk(listener, tree)

        self._perform_cross_validation(listener.result)

        return listener.result

    def _perform_cross_validation(
        self, result: DoConcurrentLocalityValidationResult
    ):
        """Perform validation requiring cross-construct analysis."""
        for construct in result.do_concurrent_constructs:
            if construct.depth > 0:
                result.diagnostics.append(
                    SemanticDiagnostic(
                        severity=DiagnosticSeverity.INFO,
                        code="LOC_I002",
                        message="Nested DO CONCURRENT detected. "
                        "Per ISO/IEC 1539-1:2018 Section 11.1.7.5, nested "
                        "DO CONCURRENT constructs must maintain independence "
                        "across all levels of nesting. Locality specifiers "
                        "at each level apply only to that level.",
                        line=construct.line,
                        column=construct.column,
                        iso_section="11.1.7.5",
                    )
                )

    def validate_file(self, filepath: str) -> DoConcurrentLocalityValidationResult:
        """Validate a Fortran 2018 file for DO CONCURRENT with locality."""
        path = Path(filepath)
        if not path.exists():
            result = DoConcurrentLocalityValidationResult()
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


def validate_do_concurrent_locality(
    code: str,
) -> DoConcurrentLocalityValidationResult:
    """Convenience function to validate DO CONCURRENT with locality."""
    validator = F2018DoConcurrentLocalityValidator()
    return validator.validate_code(code)
