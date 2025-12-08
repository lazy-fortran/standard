#!/usr/bin/env python3
"""Fortran 2003 Semantic Validator for C Interoperability and IEEE Arithmetic

Validates Fortran 2003 parse trees against semantic rules from ISO/IEC 1539-1:2004:
- Section 15: C interoperability (BIND(C), VALUE, ISO_C_BINDING types)
- Section 14: IEEE arithmetic modules (IEEE_EXCEPTIONS, IEEE_ARITHMETIC, IEEE_FEATURES)

This module implements semantic checks that go beyond syntactic parsing, enforcing
the standard's requirements for type interoperability and IEEE module usage.
"""

import sys
from dataclasses import dataclass, field
from enum import Enum, auto
from pathlib import Path
from typing import Dict, List, Optional, Set, Tuple

sys.path.insert(
    0, str(Path(__file__).parent.parent / "grammars" / "generated" / "modern")
)

from antlr4 import CommonTokenStream, InputStream, ParseTreeWalker
from Fortran2003Lexer import Fortran2003Lexer
from Fortran2003Parser import Fortran2003Parser
from Fortran2003ParserListener import Fortran2003ParserListener


class DiagnosticSeverity(Enum):
    """Severity levels for semantic diagnostics."""

    ERROR = auto()
    WARNING = auto()
    INFO = auto()


@dataclass
class SemanticDiagnostic:
    """A semantic issue detected during validation.

    Attributes:
        severity: ERROR, WARNING, or INFO level
        code: Unique identifier for this diagnostic type
        message: Human-readable description of the issue
        line: Source line number (1-based) or None if unknown
        column: Source column number (0-based) or None if unknown
        iso_section: ISO/IEC 1539-1:2004 section reference
    """

    severity: DiagnosticSeverity
    code: str
    message: str
    line: Optional[int] = None
    column: Optional[int] = None
    iso_section: Optional[str] = None


@dataclass
class ValidationResult:
    """Result of semantic validation.

    Attributes:
        diagnostics: List of all semantic issues found
        c_interop_entities: Names of entities declared with BIND(C)
        ieee_modules_used: Set of IEEE module names imported
        iso_c_binding_imports: Set of ISO_C_BINDING entities imported
    """

    diagnostics: List[SemanticDiagnostic] = field(default_factory=list)
    c_interop_entities: List[str] = field(default_factory=list)
    ieee_modules_used: Set[str] = field(default_factory=set)
    iso_c_binding_imports: Set[str] = field(default_factory=set)

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


# ISO/IEC 1539-1:2004 Section 15.2.2, Table 15.2: C interoperable types
C_INTEROPERABLE_TYPES: Set[str] = {
    # Integer types
    "c_int",
    "c_short",
    "c_long",
    "c_long_long",
    "c_signed_char",
    "c_size_t",
    "c_int8_t",
    "c_int16_t",
    "c_int32_t",
    "c_int64_t",
    "c_int_least8_t",
    "c_int_least16_t",
    "c_int_least32_t",
    "c_int_least64_t",
    "c_int_fast8_t",
    "c_int_fast16_t",
    "c_int_fast32_t",
    "c_int_fast64_t",
    "c_intmax_t",
    "c_intptr_t",
    # Floating-point types
    "c_float",
    "c_double",
    "c_long_double",
    "c_float_complex",
    "c_double_complex",
    "c_long_double_complex",
    # Other types
    "c_bool",
    "c_char",
    # Pointer types
    "c_ptr",
    "c_funptr",
}

# ISO/IEC 1539-1:2004 Section 15.2.3: C pointer constants
C_POINTER_CONSTANTS: Set[str] = {"c_null_ptr", "c_null_funptr"}

# ISO/IEC 1539-1:2004 Section 14: IEEE module entities
IEEE_MODULES: Set[str] = {"ieee_exceptions", "ieee_arithmetic", "ieee_features"}

# IEEE exception flags (Section 14.2)
IEEE_EXCEPTION_FLAGS: Set[str] = {
    "ieee_overflow",
    "ieee_underflow",
    "ieee_divide_by_zero",
    "ieee_invalid",
    "ieee_inexact",
}

# IEEE special values (Section 14.3)
IEEE_SPECIAL_VALUES: Set[str] = {
    "ieee_positive_inf",
    "ieee_negative_inf",
    "ieee_quiet_nan",
    "ieee_signaling_nan",
}

# IEEE rounding modes (Section 14.3)
IEEE_ROUNDING_MODES: Set[str] = {
    "ieee_nearest",
    "ieee_to_zero",
    "ieee_up",
    "ieee_down",
}

# IEEE features (Section 14.4)
IEEE_FEATURES: Set[str] = {
    "ieee_datatype",
    "ieee_denormal",
    "ieee_divide",
    "ieee_halting",
    "ieee_inexact_flag",
    "ieee_inf",
    "ieee_invalid_flag",
    "ieee_nan",
    "ieee_rounding",
    "ieee_sqrt",
    "ieee_underflow_flag",
}

# All IEEE entities for validation
ALL_IEEE_ENTITIES: Set[str] = (
    IEEE_EXCEPTION_FLAGS | IEEE_SPECIAL_VALUES | IEEE_ROUNDING_MODES | IEEE_FEATURES
)


class F2003SemanticListener(Fortran2003ParserListener):
    """ANTLR listener that collects semantic information from parse trees.

    Walks the parse tree and extracts:
    - BIND(C) declarations and their associated types
    - USE statements for ISO_C_BINDING and IEEE modules
    - VALUE attribute usage
    - C interoperable type declarations
    """

    def __init__(self):
        super().__init__()
        self.result = ValidationResult()
        self._current_bind_c_context: Optional[str] = None
        self._iso_c_binding_imported = False
        self._ieee_modules_imported: Set[str] = set()
        self._in_bind_c_procedure = False
        self._current_procedure_name: Optional[str] = None
        self._value_attributes: List[Tuple[str, int, int]] = []
        self._c_types_used: List[Tuple[str, int, int]] = []

    def _get_token_text(self, ctx) -> str:
        """Extract normalized text from a context."""
        if ctx is None:
            return ""
        return ctx.getText().lower()

    def _get_location(self, ctx) -> Tuple[Optional[int], Optional[int]]:
        """Extract line and column from a context."""
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
        """Add a diagnostic to the result."""
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

    def enterUse_stmt(self, ctx):
        """Check USE statements for ISO_C_BINDING and IEEE modules."""
        text = self._get_token_text(ctx)

        # Check for ISO_C_BINDING
        if "iso_c_binding" in text:
            self._iso_c_binding_imported = True
            # Extract imported entities from ONLY clause if present
            if "only" in text:
                for c_type in C_INTEROPERABLE_TYPES | C_POINTER_CONSTANTS:
                    if c_type in text:
                        self.result.iso_c_binding_imports.add(c_type)
            else:
                # Full import - all entities available
                self.result.iso_c_binding_imports = (
                    C_INTEROPERABLE_TYPES | C_POINTER_CONSTANTS
                )

        # Check for IEEE modules
        for ieee_mod in IEEE_MODULES:
            if ieee_mod in text:
                self._ieee_modules_imported.add(ieee_mod)
                self.result.ieee_modules_used.add(ieee_mod)

    def enterBinding_spec(self, ctx):
        """Track entry into BIND(C) context."""
        text = self._get_token_text(ctx)
        if "bind" in text and "c" in text:
            self._in_bind_c_procedure = True

    def exitBinding_spec(self, ctx):
        """Track exit from BIND(C) context."""
        pass

    def enterFunction_stmt(self, ctx):
        """Track function declarations with BIND(C)."""
        text = self._get_token_text(ctx)
        if "bind" in text:
            self._in_bind_c_procedure = True
            # Extract function name
            if hasattr(ctx, "identifier_or_keyword") and ctx.identifier_or_keyword:
                name = ctx.identifier_or_keyword().getText()
                self._current_procedure_name = name
                self.result.c_interop_entities.append(name)

    def exitFunction_stmt(self, ctx):
        """Reset function context."""
        pass

    def enterSubroutine_stmt(self, ctx):
        """Track subroutine declarations with BIND(C)."""
        text = self._get_token_text(ctx)
        if "bind" in text:
            self._in_bind_c_procedure = True

    def enterSubroutine_stmt_f2003(self, ctx):
        """Track F2003 subroutine declarations with BIND(C)."""
        text = self._get_token_text(ctx)
        if "bind" in text:
            self._in_bind_c_procedure = True
            # Try to extract subroutine name
            self._extract_procedure_name_from_text(text, ctx)

    def _extract_procedure_name_from_text(self, text: str, ctx):
        """Extract procedure name from statement text."""
        # Look for subroutine keyword followed by name
        if "subroutine" in text:
            parts = text.split("subroutine")
            if len(parts) > 1:
                name_part = parts[1].strip()
                # Get first identifier-like string before ( or whitespace
                name = ""
                for char in name_part:
                    if char.isalnum() or char == "_":
                        name += char
                    else:
                        break
                if name:
                    self._current_procedure_name = name
                    self.result.c_interop_entities.append(name)

    def enterDerived_type_stmt_f2003(self, ctx):
        """Check derived type declarations for BIND(C)."""
        text = self._get_token_text(ctx)
        if "bind" in text:
            self._in_bind_c_procedure = True

    def enterType_attr_spec(self, ctx):
        """Track type attributes including BIND(C)."""
        text = self._get_token_text(ctx)
        if "bind" in text:
            self._in_bind_c_procedure = True

    def exitEnd_function_stmt(self, ctx):
        """Reset on function end."""
        self._in_bind_c_procedure = False
        self._current_procedure_name = None

    def exitEnd_subroutine_stmt(self, ctx):
        """Reset on subroutine end."""
        self._in_bind_c_procedure = False
        self._current_procedure_name = None

    def exitEnd_type_stmt(self, ctx):
        """Reset on type end."""
        self._in_bind_c_procedure = False

    def exitEnd_type_stmt_f2003(self, ctx):
        """Reset on F2003 type end."""
        self._in_bind_c_procedure = False

    def enterAttr_spec(self, ctx):
        """Check for VALUE attribute usage."""
        text = self._get_token_text(ctx)
        if "value" in text:
            line, col = self._get_location(ctx)
            self._value_attributes.append(("value", line or 0, col or 0))
            # VALUE requires ISO_C_BINDING context per Section 15.3.5
            if not self._in_bind_c_procedure:
                self._add_diagnostic(
                    DiagnosticSeverity.WARNING,
                    "C_INTEROP_W001",
                    "VALUE attribute used outside BIND(C) context. "
                    "Per ISO/IEC 1539-1:2004 Section 15.3.5, VALUE is "
                    "primarily intended for C interoperability.",
                    ctx,
                    "15.3.5",
                )

    def enterC_interop_type(self, ctx):
        """Track C interoperable type usage."""
        text = self._get_token_text(ctx)
        line, col = self._get_location(ctx)
        self._c_types_used.append((text, line or 0, col or 0))

        # Check if ISO_C_BINDING was imported
        if not self._iso_c_binding_imported:
            self._add_diagnostic(
                DiagnosticSeverity.ERROR,
                "C_INTEROP_E001",
                f"C interoperable type '{text}' used without importing "
                "ISO_C_BINDING. Add: USE, INTRINSIC :: ISO_C_BINDING",
                ctx,
                "15.2.1",
            )

    def enterIeee_entity(self, ctx):
        """Validate IEEE entity usage."""
        text = self._get_token_text(ctx)
        line, col = self._get_location(ctx)

        # Check if appropriate IEEE module was imported
        if text in IEEE_EXCEPTION_FLAGS:
            if "ieee_exceptions" not in self._ieee_modules_imported:
                if "ieee_arithmetic" not in self._ieee_modules_imported:
                    self._add_diagnostic(
                        DiagnosticSeverity.ERROR,
                        "IEEE_E001",
                        f"IEEE exception flag '{text}' used without importing "
                        "IEEE_EXCEPTIONS or IEEE_ARITHMETIC module.",
                        ctx,
                        "14.2",
                    )
        elif text in IEEE_SPECIAL_VALUES or text in IEEE_ROUNDING_MODES:
            if "ieee_arithmetic" not in self._ieee_modules_imported:
                self._add_diagnostic(
                    DiagnosticSeverity.ERROR,
                    "IEEE_E002",
                    f"IEEE arithmetic entity '{text}' used without importing "
                    "IEEE_ARITHMETIC module.",
                    ctx,
                    "14.3",
                )
        elif text in IEEE_FEATURES:
            if "ieee_features" not in self._ieee_modules_imported:
                self._add_diagnostic(
                    DiagnosticSeverity.ERROR,
                    "IEEE_E003",
                    f"IEEE feature '{text}' used without importing "
                    "IEEE_FEATURES module.",
                    ctx,
                    "14.4",
                )


class F2003SemanticValidator:
    """Semantic validator for Fortran 2003 C interoperability and IEEE arithmetic.

    Performs semantic analysis on parse trees to enforce ISO/IEC 1539-1:2004
    requirements beyond what the grammar can express syntactically.

    Example usage:
        validator = F2003SemanticValidator()
        result = validator.validate_code(fortran_source_code)
        if result.has_errors:
            for diag in result.diagnostics:
                print(f"{diag.line}: {diag.message}")
    """

    def __init__(self):
        self._lexer = None
        self._parser = None

    def validate_code(self, code: str) -> ValidationResult:
        """Validate Fortran 2003 source code for semantic correctness.

        Args:
            code: Fortran 2003 source code to validate

        Returns:
            ValidationResult containing diagnostics and extracted information
        """
        # Parse the code
        input_stream = InputStream(code)
        self._lexer = Fortran2003Lexer(input_stream)
        token_stream = CommonTokenStream(self._lexer)
        self._parser = Fortran2003Parser(token_stream)

        # Get parse tree
        tree = self._parser.program_unit_f2003()

        # Check for syntax errors first
        syntax_errors = self._parser.getNumberOfSyntaxErrors()
        result = ValidationResult()

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

        # Walk tree with semantic listener
        listener = F2003SemanticListener()
        walker = ParseTreeWalker()
        walker.walk(listener, tree)

        # Perform additional cross-cutting validations
        self._validate_c_interop_consistency(listener)
        self._validate_ieee_module_consistency(listener)

        return listener.result

    def _validate_c_interop_consistency(self, listener: F2003SemanticListener):
        """Check cross-cutting C interoperability rules."""
        # Check that C types are only used with proper imports
        if listener._c_types_used and not listener._iso_c_binding_imported:
            for type_name, line, col in listener._c_types_used:
                if type_name not in listener.result.iso_c_binding_imports:
                    # Already reported in enterC_interop_type
                    pass

        # Validate VALUE attribute usage in BIND(C) procedures
        # Additional checks could be added here for:
        # - Checking that BIND(C) derived types only contain interoperable components
        # - Verifying that procedure dummy arguments have correct attributes

    def _validate_ieee_module_consistency(self, listener: F2003SemanticListener):
        """Check cross-cutting IEEE arithmetic rules."""
        # Verify that IEEE features are used only after appropriate USE statements
        # Additional checks could be added here for:
        # - Verifying IEEE function call contexts
        # - Checking IEEE flag manipulation sequences

    def validate_file(self, filepath: str) -> ValidationResult:
        """Validate a Fortran 2003 source file.

        Args:
            filepath: Path to the Fortran source file

        Returns:
            ValidationResult containing diagnostics and extracted information
        """
        path = Path(filepath)
        if not path.exists():
            result = ValidationResult()
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


def validate_c_interoperability(code: str) -> ValidationResult:
    """Convenience function to validate C interoperability in Fortran 2003 code.

    Args:
        code: Fortran 2003 source code

    Returns:
        ValidationResult with C interoperability diagnostics
    """
    validator = F2003SemanticValidator()
    return validator.validate_code(code)


def validate_ieee_arithmetic(code: str) -> ValidationResult:
    """Convenience function to validate IEEE arithmetic usage in Fortran 2003 code.

    Args:
        code: Fortran 2003 source code

    Returns:
        ValidationResult with IEEE arithmetic diagnostics
    """
    validator = F2003SemanticValidator()
    return validator.validate_code(code)


if __name__ == "__main__":
    # Example usage
    test_code = """
module test_interop
    use iso_c_binding, only: c_int, c_float
    implicit none

    type, bind(c) :: my_c_struct
        integer(c_int) :: x
        real(c_float) :: y
    end type my_c_struct

contains

    subroutine my_c_sub(n) bind(c, name="my_c_sub")
        integer(c_int), value :: n
    end subroutine my_c_sub

end module test_interop
"""

    print("Fortran 2003 Semantic Validator")
    print("=" * 40)

    validator = F2003SemanticValidator()
    result = validator.validate_code(test_code)

    print(f"Errors: {result.error_count}")
    print(f"Warnings: {result.warning_count}")
    print(f"C interop entities: {result.c_interop_entities}")
    print(f"IEEE modules used: {result.ieee_modules_used}")
    print(f"ISO_C_BINDING imports: {result.iso_c_binding_imports}")

    if result.diagnostics:
        print("\nDiagnostics:")
        for diag in result.diagnostics:
            print(f"  [{diag.severity.name}] {diag.code}: {diag.message}")
            if diag.line:
                print(f"    Line {diag.line}, Column {diag.column}")
            if diag.iso_section:
                print(f"    ISO Section: {diag.iso_section}")
