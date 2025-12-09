#!/usr/bin/env python3
"""Fortran 2008 DO CONCURRENT Semantic Validator

Implements semantic validation for Fortran 2008 DO CONCURRENT construct per
ISO/IEC 1539-1:2010 (Fortran 2008 standard).

This validator checks:
- DO CONCURRENT construct structure (Section 8.1.6.6)
- Iteration independence constraints (Section 8.1.6.6.4)
- Variable assignment restrictions (Section 8.1.6.6.4)
- Side-effect restrictions (Section 8.1.6.6.4)
- Coarray interaction rules (Sections 8.1.6.6.4, 8.5)

Reference: ISO/IEC 1539-1:2010 (Fortran 2008 International Standard)
"""

import re
import sys
from dataclasses import dataclass, field
from enum import Enum, auto
from pathlib import Path
from typing import List, Optional, Set, Tuple

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
class DoConcurrentConstruct:
    """Represents a DO CONCURRENT construct for semantic analysis."""
    index_variables: List[str] = field(default_factory=list)
    has_mask: bool = False
    mask_expr: Optional[str] = None
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
class DoConcurrentValidationResult:
    """Results from DO CONCURRENT semantic validation."""
    diagnostics: List[SemanticDiagnostic] = field(default_factory=list)
    do_concurrent_constructs: List[DoConcurrentConstruct] = field(
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


class F2008DoConcurrentListener(Fortran2008ParserListener):
    """ANTLR listener for F2008 DO CONCURRENT semantic analysis."""

    def __init__(self):
        super().__init__()
        self.result = DoConcurrentValidationResult()
        self._do_concurrent_stack: List[DoConcurrentConstruct] = []
        self._in_do_concurrent = False
        self._current_do_concurrent: Optional[DoConcurrentConstruct] = None
        self._pure_context = False
        self._impure_allowed = True

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

    def enterDo_concurrent_construct(self, ctx):
        """Enter a DO CONCURRENT construct."""
        line, col = self._get_location(ctx)
        construct = DoConcurrentConstruct(
            depth=len(self._do_concurrent_stack),
            line=line,
            column=col,
        )
        self._do_concurrent_stack.append(construct)
        self._in_do_concurrent = True
        self._current_do_concurrent = construct

    def exitDo_concurrent_construct(self, ctx):
        """Exit a DO CONCURRENT construct and perform validation."""
        if self._do_concurrent_stack:
            construct = self._do_concurrent_stack.pop()
            self.result.do_concurrent_constructs.append(construct)
            self._validate_do_concurrent_construct(construct, ctx)
            if self._do_concurrent_stack:
                self._current_do_concurrent = self._do_concurrent_stack[-1]
            else:
                self._current_do_concurrent = None
                self._in_do_concurrent = False

    def enterDo_concurrent_stmt(self, ctx):
        """Process DO CONCURRENT statement header."""
        text = self._get_token_text(ctx)
        if self._current_do_concurrent is None:
            return

        index_vars = self._extract_index_variables(text)
        self._current_do_concurrent.index_variables = index_vars

        if "," in text:
            parts = text.split(")")
            if len(parts) > 1:
                after_triplets = parts[-2] if len(parts) > 2 else ""
                if "," in after_triplets:
                    self._current_do_concurrent.has_mask = True

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

        Per ISO/IEC 1539-1:2010 Section 8.1.6.6.4, iterations of DO CONCURRENT
        must be independent - a variable defined in one iteration shall not be
        referenced in another. This method extracts identifiers that could be
        variable references to enable dependency checking.
        """
        keywords = {
            "real", "integer", "logical", "character", "complex", "double",
            "precision", "kind", "len", "size", "shape", "lbound", "ubound",
            "allocated", "associated", "present", "abs", "sqrt", "sin", "cos",
            "tan", "exp", "log", "max", "min", "mod", "nint", "floor", "ceiling",
            "sum", "product", "maxval", "minval", "any", "all", "count", "pack",
            "unpack", "merge", "spread", "reshape", "transpose", "matmul", "dot",
        }
        identifiers = set(re.findall(r"\b([a-z_]\w*)\b", text))
        return identifiers - keywords

    def enterScalar_mask_expr(self, ctx):
        """Track variable references in DO CONCURRENT mask expressions.

        Per ISO/IEC 1539-1:2010 Section 8.1.6.6, the mask expression in
        DO CONCURRENT is evaluated for each iteration. Variables referenced
        in the mask contribute to the dependency analysis for iteration
        independence validation.
        """
        if not self._in_do_concurrent or self._current_do_concurrent is None:
            return

        text = self._get_token_text(ctx)
        mask_vars = self._extract_referenced_variables(text)
        self._current_do_concurrent.referenced_variables.update(mask_vars)
        self._current_do_concurrent.has_mask = True
        self._current_do_concurrent.mask_expr = text

    def enterIf_construct(self, ctx):
        """Track variable references in IF construct within DO CONCURRENT.

        Per ISO/IEC 1539-1:2010 Section 8.1.6.6.4, iteration independence
        requires tracking all variable references, including those in
        conditional expressions. This method extracts variables from both
        IF and ELSE IF conditions.
        """
        if not self._in_do_concurrent or self._current_do_concurrent is None:
            return

        text = self._get_token_text(ctx)

        if_match = re.search(r"if\s*\((.+?)\)\s*then", text, re.IGNORECASE)
        if if_match:
            condition_text = if_match.group(1)
            condition_vars = self._extract_referenced_variables(condition_text)
            self._current_do_concurrent.referenced_variables.update(
                condition_vars
            )

        for elseif_match in re.finditer(
            r"elseif\s*\((.+?)\)\s*then", text, re.IGNORECASE
        ):
            condition_text = elseif_match.group(1)
            condition_vars = self._extract_referenced_variables(condition_text)
            self._current_do_concurrent.referenced_variables.update(
                condition_vars
            )

    def enterCall_stmt(self, ctx):
        """Track procedure calls and argument references within DO CONCURRENT.

        Per ISO/IEC 1539-1:2010 Section 8.1.6.6.4, variables passed as
        procedure arguments must be tracked for iteration independence
        analysis, as they may be modified or referenced across iterations.
        """
        if not self._in_do_concurrent or self._current_do_concurrent is None:
            return

        text = self._get_token_text(ctx)
        self._current_do_concurrent.contains_procedure_call = True

        call_match = re.search(r"call\s*(\w+)", text)
        if call_match:
            proc_name = call_match.group(1)
            self._current_do_concurrent.called_procedures.add(proc_name)

        args_match = re.search(r"call\s*\w+\s*\((.+)\)", text, re.IGNORECASE)
        if args_match:
            args_text = args_match.group(1)
            arg_vars = self._extract_referenced_variables(args_text)
            self._current_do_concurrent.referenced_variables.update(arg_vars)

    def enterPrint_stmt(self, ctx):
        """Track PRINT statements within DO CONCURRENT."""
        if not self._in_do_concurrent or self._current_do_concurrent is None:
            return
        self._current_do_concurrent.contains_io = True
        self._add_diagnostic(
            DiagnosticSeverity.WARNING,
            "DO_CONC_W001",
            "PRINT statement within DO CONCURRENT may cause nondeterministic "
            "output ordering. Per ISO/IEC 1539-1:2010 Section 8.1.6.6.4, "
            "I/O operations are not recommended in DO CONCURRENT.",
            ctx,
            "8.1.6.6.4",
        )

    def enterStop_stmt(self, ctx):
        """Track STOP statements within DO CONCURRENT."""
        if not self._in_do_concurrent or self._current_do_concurrent is None:
            return
        self._current_do_concurrent.contains_stop = True
        self._add_diagnostic(
            DiagnosticSeverity.ERROR,
            "DO_CONC_E001",
            "STOP statement is prohibited within DO CONCURRENT. "
            "Per ISO/IEC 1539-1:2010 Section 8.1.6.6.4, a STOP or ERROR STOP "
            "statement shall not appear within a DO CONCURRENT construct.",
            ctx,
            "8.1.6.6.4",
        )

    def enterError_stop_stmt(self, ctx):
        """Track ERROR STOP statements within DO CONCURRENT."""
        if not self._in_do_concurrent or self._current_do_concurrent is None:
            return
        self._current_do_concurrent.contains_stop = True
        self._add_diagnostic(
            DiagnosticSeverity.ERROR,
            "DO_CONC_E002",
            "ERROR STOP statement is prohibited within DO CONCURRENT. "
            "Per ISO/IEC 1539-1:2010 Section 8.1.6.6.4, a STOP or ERROR STOP "
            "statement shall not appear within a DO CONCURRENT construct.",
            ctx,
            "8.1.6.6.4",
        )

    def enterSync_all_stmt(self, ctx):
        """Track SYNC ALL statements within DO CONCURRENT."""
        if not self._in_do_concurrent or self._current_do_concurrent is None:
            return
        self._current_do_concurrent.contains_sync = True
        self._add_diagnostic(
            DiagnosticSeverity.ERROR,
            "DO_CONC_E003",
            "Image control statement SYNC ALL is prohibited within "
            "DO CONCURRENT. Per ISO/IEC 1539-1:2010 Section 8.1.6.6.4, "
            "an image control statement shall not appear within a "
            "DO CONCURRENT construct.",
            ctx,
            "8.1.6.6.4",
        )

    def enterSync_images_stmt(self, ctx):
        """Track SYNC IMAGES statements within DO CONCURRENT."""
        if not self._in_do_concurrent or self._current_do_concurrent is None:
            return
        self._current_do_concurrent.contains_sync = True
        self._add_diagnostic(
            DiagnosticSeverity.ERROR,
            "DO_CONC_E004",
            "Image control statement SYNC IMAGES is prohibited within "
            "DO CONCURRENT. Per ISO/IEC 1539-1:2010 Section 8.1.6.6.4, "
            "an image control statement shall not appear within a "
            "DO CONCURRENT construct.",
            ctx,
            "8.1.6.6.4",
        )

    def enterSync_memory_stmt(self, ctx):
        """Track SYNC MEMORY statements within DO CONCURRENT."""
        if not self._in_do_concurrent or self._current_do_concurrent is None:
            return
        self._current_do_concurrent.contains_sync = True
        self._add_diagnostic(
            DiagnosticSeverity.ERROR,
            "DO_CONC_E005",
            "Image control statement SYNC MEMORY is prohibited within "
            "DO CONCURRENT. Per ISO/IEC 1539-1:2010 Section 8.1.6.6.4, "
            "an image control statement shall not appear within a "
            "DO CONCURRENT construct.",
            ctx,
            "8.1.6.6.4",
        )

    def enterAllocate_stmt_f2008(self, ctx):
        """Track ALLOCATE statements within DO CONCURRENT."""
        if not self._in_do_concurrent or self._current_do_concurrent is None:
            return
        self._current_do_concurrent.contains_allocate = True
        text = self._get_token_text(ctx)
        if "[" in text:
            self._current_do_concurrent.contains_coarray_ref = True
            self._add_diagnostic(
                DiagnosticSeverity.ERROR,
                "DO_CONC_E006",
                "ALLOCATE with coarray is prohibited within DO CONCURRENT. "
                "Per ISO/IEC 1539-1:2010 Section 8.1.6.6.4, allocation of "
                "coarrays shall not appear within a DO CONCURRENT construct.",
                ctx,
                "8.1.6.6.4",
            )

    def enterLhs_expression(self, ctx):
        """Track coarray references within DO CONCURRENT."""
        if not self._in_do_concurrent or self._current_do_concurrent is None:
            return
        text = self._get_token_text(ctx)
        if "[" in text and "]" in text:
            self._current_do_concurrent.contains_coarray_ref = True
            if re.search(r"\[\s*\d+\s*\]", text):
                self._add_diagnostic(
                    DiagnosticSeverity.WARNING,
                    "DO_CONC_W002",
                    "Coarray reference with explicit image selector within "
                    "DO CONCURRENT. Per ISO/IEC 1539-1:2010 Section 8.1.6.6.4, "
                    "remote coarray access may violate iteration independence.",
                    ctx,
                    "8.1.6.6.4",
                )

    def _validate_do_concurrent_construct(
        self, construct: DoConcurrentConstruct, ctx
    ):
        """Validate a completed DO CONCURRENT construct."""
        self._validate_index_variable_usage(construct, ctx)
        self._validate_variable_dependencies(construct, ctx)

    def _validate_index_variable_usage(
        self, construct: DoConcurrentConstruct, ctx
    ):
        """Check that index variables are not illegally assigned."""
        for idx_var in construct.index_variables:
            if idx_var in construct.assigned_variables:
                self._add_diagnostic(
                    DiagnosticSeverity.ERROR,
                    "DO_CONC_E007",
                    f"Index variable '{idx_var}' shall not be assigned within "
                    "DO CONCURRENT. Per ISO/IEC 1539-1:2010 Section 8.1.6.6.4, "
                    "the index variable shall not be redefined within the loop.",
                    ctx,
                    "8.1.6.6.4",
                )

    def _validate_variable_dependencies(
        self, construct: DoConcurrentConstruct, ctx
    ):
        """Check for potential variable dependency violations."""
        assigned = construct.assigned_variables
        if len(assigned) == 0:
            return

        assigned_non_index = assigned - set(construct.index_variables)
        for var in assigned_non_index:
            if var in construct.referenced_variables:
                self._add_diagnostic(
                    DiagnosticSeverity.INFO,
                    "DO_CONC_I001",
                    f"Variable '{var}' is both assigned and referenced within "
                    "DO CONCURRENT. Per ISO/IEC 1539-1:2010 Section 8.1.6.6.4, "
                    "ensure iterations are independent - a variable defined in "
                    "one iteration shall not be referenced in another.",
                    ctx,
                    "8.1.6.6.4",
                )


class F2008DoConcurrentValidator:
    """Semantic validator for Fortran 2008 DO CONCURRENT construct."""

    def __init__(self):
        self._lexer = None
        self._parser = None

    def validate_code(self, code: str) -> DoConcurrentValidationResult:
        """Validate Fortran 2008 code for DO CONCURRENT semantics."""
        input_stream = InputStream(code)
        self._lexer = Fortran2008Lexer(input_stream)
        token_stream = CommonTokenStream(self._lexer)
        self._parser = Fortran2008Parser(token_stream)

        tree = self._parser.program_unit_f2008()

        syntax_errors = self._parser.getNumberOfSyntaxErrors()
        result = DoConcurrentValidationResult()

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

        listener = F2008DoConcurrentListener()
        walker = ParseTreeWalker()
        walker.walk(listener, tree)

        self._perform_cross_validation(listener.result)

        return listener.result

    def _perform_cross_validation(self, result: DoConcurrentValidationResult):
        """Perform validation requiring cross-construct analysis."""
        for construct in result.do_concurrent_constructs:
            if construct.depth > 0:
                result.diagnostics.append(
                    SemanticDiagnostic(
                        severity=DiagnosticSeverity.INFO,
                        code="DO_CONC_I002",
                        message="Nested DO CONCURRENT detected. "
                        "Per ISO/IEC 1539-1:2010 Section 8.1.6.6.4, nested "
                        "DO CONCURRENT constructs must maintain independence "
                        "across all levels of nesting.",
                        line=construct.line,
                        column=construct.column,
                        iso_section="8.1.6.6.4",
                    )
                )

    def validate_file(self, filepath: str) -> DoConcurrentValidationResult:
        """Validate a Fortran 2008 file for DO CONCURRENT semantics."""
        path = Path(filepath)
        if not path.exists():
            result = DoConcurrentValidationResult()
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


def validate_do_concurrent(code: str) -> DoConcurrentValidationResult:
    """Convenience function to validate DO CONCURRENT semantics."""
    validator = F2008DoConcurrentValidator()
    return validator.validate_code(code)
