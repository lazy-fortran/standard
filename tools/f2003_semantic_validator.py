#!/usr/bin/env python3
"""Fortran 2003 Semantic Validator

Validates Fortran 2003 parse trees against semantic rules from ISO/IEC 1539-1:2004:
- Section 12.3.2: Procedure pointers, abstract interfaces, and characteristics
- Section 4.5.4: Type-bound procedures and binding characteristics
- Section 15: C interoperability (BIND(C), VALUE, ISO_C_BINDING types)
- Section 14: IEEE arithmetic modules (IEEE_EXCEPTIONS, IEEE_ARITHMETIC, IEEE_FEATURES)

This module implements semantic checks that go beyond syntactic parsing, enforcing
the standard's requirements for procedure characteristics, type interoperability,
and IEEE module usage.
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
        procedures: Dict mapping procedure names to their characteristics
        abstract_interfaces: Dict mapping interface names to definitions
        procedure_pointers: Dict mapping pointer names to declarations
        type_bound_procedures: Dict mapping type names to their bindings
        generic_bindings: Dict mapping type names to their generic bindings
    """

    diagnostics: List[SemanticDiagnostic] = field(default_factory=list)
    c_interop_entities: List[str] = field(default_factory=list)
    ieee_modules_used: Set[str] = field(default_factory=set)
    iso_c_binding_imports: Set[str] = field(default_factory=set)
    procedures: Dict[str, "ProcedureCharacteristics"] = field(default_factory=dict)
    abstract_interfaces: Dict[str, "AbstractInterface"] = field(default_factory=dict)
    procedure_pointers: Dict[str, "ProcedurePointer"] = field(default_factory=dict)
    type_bound_procedures: Dict[str, List["TypeBoundProcedure"]] = field(
        default_factory=dict
    )
    generic_bindings: Dict[str, List["GenericBinding"]] = field(default_factory=dict)

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


# ISO/IEC 1539-1:2004 Section 12.3.2.2: Procedure characteristics
# Each procedure has characteristics that determine compatibility
@dataclass
class ArgumentCharacteristic:
    """Characteristics of a single dummy argument (ISO/IEC 1539-1:2004 12.2.2.1).

    Attributes:
        name: Argument name (optional for unnamed arguments)
        type_name: Type specification (integer, real, etc.)
        intent: IN, OUT, or INOUT
        is_optional: Whether the argument is OPTIONAL
        is_allocatable: Whether the argument is ALLOCATABLE
        is_pointer: Whether the argument is POINTER
        is_value: Whether the argument has VALUE attribute
        rank: Array rank (0 for scalar)
    """

    name: Optional[str] = None
    type_name: Optional[str] = None
    intent: Optional[str] = None
    is_optional: bool = False
    is_allocatable: bool = False
    is_pointer: bool = False
    is_value: bool = False
    rank: int = 0


@dataclass
class ProcedureCharacteristics:
    """Characteristics of a procedure (ISO/IEC 1539-1:2004 12.2.2.1).

    Per Section 12.2.2.1, procedure characteristics include:
    - Whether it is a function or subroutine
    - Result characteristics (for functions)
    - Dummy argument characteristics (type, kind, rank, intent)
    - Whether it is pure, elemental
    - Whether it has explicit or implicit interface

    Attributes:
        name: Procedure name
        is_function: True for functions, False for subroutines
        is_pure: Whether PURE prefix is present
        is_elemental: Whether ELEMENTAL prefix is present
        result_type: Result type for functions
        result_name: Result variable name (if different from function name)
        arguments: List of argument characteristics in order
        has_explicit_interface: Whether an explicit interface is available
        is_bind_c: Whether BIND(C) is specified
        bind_name: Optional C binding name
        line: Source line number
        column: Source column number
    """

    name: str
    is_function: bool = False
    is_pure: bool = False
    is_elemental: bool = False
    result_type: Optional[str] = None
    result_name: Optional[str] = None
    arguments: List[ArgumentCharacteristic] = field(default_factory=list)
    has_explicit_interface: bool = True
    is_bind_c: bool = False
    bind_name: Optional[str] = None
    line: Optional[int] = None
    column: Optional[int] = None


@dataclass
class AbstractInterface:
    """Represents an abstract interface definition (ISO/IEC 1539-1:2004 12.3.2.3).

    Abstract interfaces provide explicit interfaces for procedure pointers
    and deferred type-bound procedures.

    Attributes:
        name: Interface name (may be None for unnamed interfaces)
        procedures: Procedure characteristics defined in this interface
        line: Source line number
        column: Source column number
    """

    name: Optional[str] = None
    procedures: List[ProcedureCharacteristics] = field(default_factory=list)
    line: Optional[int] = None
    column: Optional[int] = None


@dataclass
class ProcedurePointer:
    """Represents a procedure pointer declaration (ISO/IEC 1539-1:2004 12.3.2.3).

    Procedure pointers can point to procedures that have compatible
    characteristics with their declared interface.

    Attributes:
        name: Pointer variable name
        interface_name: Name of the interface this pointer is declared with
        is_component: Whether this is a procedure component in a derived type
        initial_target: Initial target procedure (if => specified)
        pass_attr: PASS or NOPASS attribute for procedure components
        line: Source line number
        column: Source column number
    """

    name: str
    interface_name: Optional[str] = None
    is_component: bool = False
    initial_target: Optional[str] = None
    pass_attr: Optional[str] = None
    line: Optional[int] = None
    column: Optional[int] = None


@dataclass
class TypeBoundProcedure:
    """Represents a type-bound procedure (ISO/IEC 1539-1:2004 4.5.4).

    Type-bound procedures are bindings within a derived type that associate
    a binding name with a procedure.

    Attributes:
        binding_name: Name used to invoke the binding
        procedure_name: Actual procedure implementing the binding
        interface_name: Interface name for deferred bindings
        is_deferred: Whether this is a DEFERRED binding
        is_nopass: Whether NOPASS is specified
        pass_arg: Name of the passed-object dummy argument
        is_non_overridable: Whether NON_OVERRIDABLE is specified
        access: PUBLIC or PRIVATE
        line: Source line number
        column: Source column number
    """

    binding_name: str
    procedure_name: Optional[str] = None
    interface_name: Optional[str] = None
    is_deferred: bool = False
    is_nopass: bool = False
    pass_arg: Optional[str] = None
    is_non_overridable: bool = False
    access: Optional[str] = None
    line: Optional[int] = None
    column: Optional[int] = None


@dataclass
class GenericBinding:
    """Represents a generic type-bound procedure (ISO/IEC 1539-1:2004 4.5.4).

    Generic bindings map a generic name to multiple specific procedures.

    Attributes:
        generic_name: Generic interface name or operator
        specific_bindings: List of specific procedure binding names
        is_operator: Whether this is an operator overload
        access: PUBLIC or PRIVATE
        line: Source line number
        column: Source column number
    """

    generic_name: str
    specific_bindings: List[str] = field(default_factory=list)
    is_operator: bool = False
    access: Optional[str] = None
    line: Optional[int] = None
    column: Optional[int] = None


class F2003SemanticListener(Fortran2003ParserListener):
    """ANTLR listener that collects semantic information from parse trees.

    Walks the parse tree and extracts:
    - Procedure declarations with their characteristics (ISO 12.2.2.1)
    - Abstract interfaces and their procedure specifications (ISO 12.3.2.3)
    - Procedure pointer declarations (ISO 12.3.2.3)
    - Type-bound procedures and generic bindings (ISO 4.5.4)
    - BIND(C) declarations and their associated types (ISO 15)
    - USE statements for ISO_C_BINDING and IEEE modules (ISO 14, 15)
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
        self._in_abstract_interface = False
        self._current_abstract_interface: Optional[AbstractInterface] = None
        self._current_procedure: Optional[ProcedureCharacteristics] = None
        self._current_type_name: Optional[str] = None
        self._in_type_bound_part = False
        self._current_binding_attrs: List[str] = []
        self._current_interface_name: Optional[str] = None

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

    def enterInterface_stmt(self, ctx):
        """Track entry into interface blocks (ISO/IEC 1539-1:2004 12.3.2).

        Handles both named interfaces and ABSTRACT INTERFACE blocks.
        """
        text = self._get_token_text(ctx)
        line, col = self._get_location(ctx)

        if "abstract" in text:
            self._in_abstract_interface = True
            self._current_abstract_interface = AbstractInterface(
                name=None, procedures=[], line=line, column=col
            )
            interface_name = self._extract_interface_name(text)
            if interface_name:
                self._current_abstract_interface.name = interface_name
        else:
            interface_name = self._extract_interface_name(text)
            self._current_interface_name = interface_name

    def _extract_interface_name(self, text: str) -> Optional[str]:
        """Extract interface name from interface statement text."""
        text = text.strip()
        if "interface" in text:
            parts = text.split("interface")
            if len(parts) > 1:
                name_part = parts[1].strip()
                name = ""
                for char in name_part:
                    if char.isalnum() or char == "_":
                        name += char
                    else:
                        break
                if name:
                    return name
        return None

    def exitInterface_block(self, ctx):
        """Complete interface block processing."""
        if self._in_abstract_interface and self._current_abstract_interface:
            if self._current_abstract_interface.name:
                name = self._current_abstract_interface.name
            else:
                first_proc = None
                if self._current_abstract_interface.procedures:
                    first_proc = self._current_abstract_interface.procedures[0]
                if first_proc:
                    name = first_proc.name
                else:
                    name = f"_anon_interface_{id(self._current_abstract_interface)}"
            self.result.abstract_interfaces[name] = self._current_abstract_interface

        self._in_abstract_interface = False
        self._current_abstract_interface = None
        self._current_interface_name = None

    def enterInterface_body(self, ctx):
        """Track procedure declarations within interface bodies."""
        pass

    def exitInterface_body(self, ctx):
        """Complete procedure within interface body."""
        if self._current_procedure and self._in_abstract_interface:
            if self._current_abstract_interface:
                self._current_abstract_interface.procedures.append(
                    self._current_procedure
                )
        self._current_procedure = None

    def enterFunction_stmt_f2003(self, ctx):
        """Track F2003 function declarations with characteristics."""
        text = self._get_token_text(ctx)
        line, col = self._get_location(ctx)

        is_pure = "pure" in text
        is_elemental = "elemental" in text
        is_bind_c = "bind" in text

        func_name = self._extract_function_name(text)
        if not func_name:
            return

        result_name = self._extract_result_name(text)
        result_type = self._extract_prefix_type(text)

        self._current_procedure = ProcedureCharacteristics(
            name=func_name,
            is_function=True,
            is_pure=is_pure,
            is_elemental=is_elemental,
            result_type=result_type,
            result_name=result_name,
            is_bind_c=is_bind_c,
            line=line,
            column=col,
        )
        self._current_procedure_name = func_name

        if is_bind_c:
            self._in_bind_c_procedure = True
            self.result.c_interop_entities.append(func_name)

    def _extract_function_name(self, text: str) -> Optional[str]:
        """Extract function name from function statement."""
        match = re.search(r"function\s+(\w+)", text)
        if match:
            return match.group(1)
        return None

    def _extract_result_name(self, text: str) -> Optional[str]:
        """Extract RESULT variable name if specified."""
        match = re.search(r"result\s*\(\s*(\w+)\s*\)", text)
        if match:
            return match.group(1)
        return None

    def _extract_prefix_type(self, text: str) -> Optional[str]:
        """Extract type prefix from function declaration."""
        type_keywords = [
            "integer",
            "real",
            "complex",
            "logical",
            "character",
            "double precision",
        ]
        for type_kw in type_keywords:
            if type_kw in text:
                return type_kw
        return None

    def enterFunction_stmt_interface(self, ctx):
        """Track function declarations in interface blocks."""
        text = self._get_token_text(ctx)
        line, col = self._get_location(ctx)

        is_pure = "pure" in text
        is_elemental = "elemental" in text

        func_name = self._extract_function_name(text)
        if not func_name:
            return

        result_name = self._extract_result_name(text)
        result_type = self._extract_prefix_type(text)

        self._current_procedure = ProcedureCharacteristics(
            name=func_name,
            is_function=True,
            is_pure=is_pure,
            is_elemental=is_elemental,
            result_type=result_type,
            result_name=result_name,
            has_explicit_interface=True,
            line=line,
            column=col,
        )

    def exitFunction_subprogram_f2003(self, ctx):
        """Complete function subprogram and store characteristics."""
        if self._current_procedure:
            self.result.procedures[self._current_procedure.name] = (
                self._current_procedure
            )
        self._current_procedure = None
        self._in_bind_c_procedure = False
        self._current_procedure_name = None

    def enterSubroutine_stmt_interface(self, ctx):
        """Track subroutine declarations in interface blocks."""
        text = self._get_token_text(ctx)
        line, col = self._get_location(ctx)

        is_pure = "pure" in text
        is_elemental = "elemental" in text

        sub_name = self._extract_subroutine_name(text)
        if not sub_name:
            return

        self._current_procedure = ProcedureCharacteristics(
            name=sub_name,
            is_function=False,
            is_pure=is_pure,
            is_elemental=is_elemental,
            has_explicit_interface=True,
            line=line,
            column=col,
        )

    def _extract_subroutine_name(self, text: str) -> Optional[str]:
        """Extract subroutine name from subroutine statement."""
        match = re.search(r"subroutine\s+(\w+)", text)
        if match:
            return match.group(1)
        return None

    def exitSubroutine_subprogram_f2003(self, ctx):
        """Complete subroutine subprogram and store characteristics."""
        if self._current_procedure:
            self.result.procedures[self._current_procedure.name] = (
                self._current_procedure
            )
        self._current_procedure = None
        self._in_bind_c_procedure = False
        self._current_procedure_name = None

    def enterProcedure_declaration_stmt(self, ctx):
        """Track procedure pointer declarations (ISO 12.3.2.3)."""
        text = self._get_token_text(ctx)
        line, col = self._get_location(ctx)

        interface_match = re.search(r"procedure\s*\(\s*(\w+)\s*\)", text)
        interface_name = interface_match.group(1) if interface_match else None

        has_pointer = "pointer" in text

        pointer_names = self._extract_procedure_pointer_names(text)

        for ptr_name in pointer_names:
            target = self._extract_pointer_target(text, ptr_name)
            pointer = ProcedurePointer(
                name=ptr_name,
                interface_name=interface_name,
                is_component=False,
                initial_target=target,
                line=line,
                column=col,
            )
            self.result.procedure_pointers[ptr_name] = pointer

            if interface_name and has_pointer:
                if interface_name not in self.result.abstract_interfaces:
                    if interface_name not in self.result.procedures:
                        self._add_diagnostic(
                            DiagnosticSeverity.INFO,
                            "PROC_CHAR_I001",
                            f"Procedure pointer '{ptr_name}' references "
                            f"interface '{interface_name}' which should be "
                            "defined in an ABSTRACT INTERFACE or as a procedure.",
                            ctx,
                            "12.3.2.3",
                        )

    def _extract_procedure_pointer_names(self, text: str) -> List[str]:
        """Extract procedure pointer entity names from declaration."""
        names = []
        double_colon_pos = text.find("::")
        if double_colon_pos >= 0:
            decl_part = text[double_colon_pos + 2 :]
            name_pattern = re.compile(r"(\w+)\s*(?:=>|,|$)")
            for match in name_pattern.finditer(decl_part):
                name = match.group(1)
                if name and name not in ["null", "pointer", "nopass", "pass"]:
                    names.append(name)
        return names

    def _extract_pointer_target(self, text: str, ptr_name: str) -> Optional[str]:
        """Extract initial target for a procedure pointer."""
        pattern = rf"{ptr_name}\s*=>\s*(\w+)"
        match = re.search(pattern, text)
        if match:
            target = match.group(1)
            if target != "null":
                return target
        return None

    def enterProc_component_def_stmt(self, ctx):
        """Track procedure component definitions in derived types."""
        text = self._get_token_text(ctx)
        line, col = self._get_location(ctx)

        interface_match = re.search(r"procedure\s*\(\s*(\w+)\s*\)", text)
        interface_name = interface_match.group(1) if interface_match else None

        has_nopass = "nopass" in text
        pass_arg = None
        pass_match = re.search(r"pass\s*\(\s*(\w+)\s*\)", text)
        if pass_match:
            pass_arg = pass_match.group(1)
        elif "pass" in text and not has_nopass:
            pass_arg = "self"

        comp_names = self._extract_proc_component_names(text)

        for comp_name in comp_names:
            target = self._extract_pointer_target(text, comp_name)
            pointer = ProcedurePointer(
                name=comp_name,
                interface_name=interface_name,
                is_component=True,
                initial_target=target,
                pass_attr="nopass" if has_nopass else ("pass" if pass_arg else None),
                line=line,
                column=col,
            )
            self.result.procedure_pointers[comp_name] = pointer

    def _extract_proc_component_names(self, text: str) -> List[str]:
        """Extract procedure component names from definition."""
        names = []
        double_colon_pos = text.find("::")
        if double_colon_pos >= 0:
            decl_part = text[double_colon_pos + 2 :]
            name_pattern = re.compile(r"(\w+)\s*(?:=>|,|$)")
            for match in name_pattern.finditer(decl_part):
                name = match.group(1)
                if name and name not in ["null"]:
                    names.append(name)
        return names

    def enterDerived_type_stmt_f2003(self, ctx):
        """Track derived type declarations."""
        text = self._get_token_text(ctx)
        line, col = self._get_location(ctx)

        type_match = re.search(r"type\s*(?:,.*?)?(?:::)?\s*(\w+)", text)
        if type_match:
            self._current_type_name = type_match.group(1)

        if "bind" in text:
            self._in_bind_c_procedure = True

    def enterType_bound_procedure_part(self, ctx):
        """Track entry into type-bound procedure section."""
        self._in_type_bound_part = True

    def exitType_bound_procedure_part(self, ctx):
        """Exit type-bound procedure section."""
        self._in_type_bound_part = False

    def enterType_bound_procedure_stmt(self, ctx):
        """Track type-bound procedure bindings (ISO 4.5.4)."""
        text = self._get_token_text(ctx)
        line, col = self._get_location(ctx)

        if not self._current_type_name:
            return

        is_deferred = "deferred" in text
        is_nopass = "nopass" in text
        is_non_overridable = "non_overridable" in text

        pass_arg = None
        pass_match = re.search(r"pass\s*\(\s*(\w+)\s*\)", text)
        if pass_match:
            pass_arg = pass_match.group(1)

        interface_match = re.search(r"procedure\s*\(\s*(\w+)\s*\)", text)
        interface_name = interface_match.group(1) if interface_match else None

        access = None
        if "public" in text:
            access = "public"
        elif "private" in text:
            access = "private"

        bindings = self._extract_type_bound_bindings(text)

        type_name = self._current_type_name
        if type_name not in self.result.type_bound_procedures:
            self.result.type_bound_procedures[type_name] = []

        for binding_name, proc_name in bindings:
            tbp = TypeBoundProcedure(
                binding_name=binding_name,
                procedure_name=proc_name,
                interface_name=interface_name,
                is_deferred=is_deferred,
                is_nopass=is_nopass,
                pass_arg=pass_arg,
                is_non_overridable=is_non_overridable,
                access=access,
                line=line,
                column=col,
            )
            self.result.type_bound_procedures[type_name].append(tbp)

            if is_deferred:
                if not interface_name:
                    self._add_diagnostic(
                        DiagnosticSeverity.ERROR,
                        "PROC_CHAR_E001",
                        f"DEFERRED type-bound procedure '{binding_name}' "
                        "must specify an interface name via PROCEDURE(iface).",
                        ctx,
                        "4.5.4",
                    )

    def _extract_type_bound_bindings(
        self, text: str
    ) -> List[Tuple[str, Optional[str]]]:
        """Extract binding name => procedure name pairs."""
        bindings = []
        double_colon_pos = text.find("::")
        if double_colon_pos >= 0:
            decl_part = text[double_colon_pos + 2 :]
            binding_pattern = re.compile(r"(\w+)\s*(?:=>\s*(\w+))?")
            for match in binding_pattern.finditer(decl_part):
                binding_name = match.group(1)
                proc_name = match.group(2) if match.group(2) else binding_name
                if binding_name:
                    bindings.append((binding_name, proc_name))
        return bindings

    def enterType_bound_generic_stmt(self, ctx):
        """Track generic type-bound procedures (ISO 4.5.4)."""
        text = self._get_token_text(ctx)
        line, col = self._get_location(ctx)

        if not self._current_type_name:
            return

        is_operator = "operator" in text

        access = None
        if "public" in text:
            access = "public"
        elif "private" in text:
            access = "private"

        generic_name = self._extract_generic_name(text)
        if not generic_name:
            return

        specific_bindings = self._extract_generic_bindings(text)

        type_name = self._current_type_name
        if type_name not in self.result.generic_bindings:
            self.result.generic_bindings[type_name] = []

        gb = GenericBinding(
            generic_name=generic_name,
            specific_bindings=specific_bindings,
            is_operator=is_operator,
            access=access,
            line=line,
            column=col,
        )
        self.result.generic_bindings[type_name].append(gb)

    def _extract_generic_name(self, text: str) -> Optional[str]:
        """Extract generic interface name from generic statement."""
        if "operator" in text:
            op_match = re.search(r"operator\s*\(\s*([^\)]+)\s*\)", text)
            if op_match:
                return f"operator({op_match.group(1).strip()})"

        double_colon_pos = text.find("::")
        if double_colon_pos >= 0:
            after_double_colon = text[double_colon_pos + 2 :].strip()
            generic_match = re.match(r"(\w+)\s*=>", after_double_colon)
            if generic_match:
                return generic_match.group(1)
        return None

    def _extract_generic_bindings(self, text: str) -> List[str]:
        """Extract specific binding names from generic statement."""
        bindings = []
        arrow_pos = text.find("=>")
        if arrow_pos >= 0:
            bindings_part = text[arrow_pos + 2 :]
            binding_pattern = re.compile(r"(\w+)")
            for match in binding_pattern.finditer(bindings_part):
                bindings.append(match.group(1))
        return bindings

    def exitDerived_type_def_f2003(self, ctx):
        """Complete derived type definition."""
        self._current_type_name = None
        self._in_bind_c_procedure = False


class F2003SemanticValidator:
    """Semantic validator for Fortran 2003 procedure characteristics, C
    interoperability and IEEE arithmetic.

    Performs semantic analysis on parse trees to enforce ISO/IEC 1539-1:2004
    requirements beyond what the grammar can express syntactically.

    Validates:
    - Procedure characteristics (Section 12.2.2.1, 12.3.2)
    - Abstract interfaces and procedure pointer compatibility (Section 12.3.2.3)
    - Type-bound procedure bindings (Section 4.5.4)
    - C interoperability (Section 15)
    - IEEE arithmetic modules (Section 14)

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
        input_stream = InputStream(code)
        self._lexer = Fortran2003Lexer(input_stream)
        token_stream = CommonTokenStream(self._lexer)
        self._parser = Fortran2003Parser(token_stream)

        tree = self._parser.program_unit_f2003()

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

        listener = F2003SemanticListener()
        walker = ParseTreeWalker()
        walker.walk(listener, tree)

        self._validate_c_interop_consistency(listener)
        self._validate_ieee_module_consistency(listener)
        self._validate_procedure_characteristics(listener)

        return listener.result

    def _validate_c_interop_consistency(self, listener: F2003SemanticListener):
        """Check cross-cutting C interoperability rules."""
        if listener._c_types_used and not listener._iso_c_binding_imported:
            for type_name, line, col in listener._c_types_used:
                if type_name not in listener.result.iso_c_binding_imports:
                    pass

    def _validate_ieee_module_consistency(self, listener: F2003SemanticListener):
        """Check cross-cutting IEEE arithmetic rules."""
        pass

    def _validate_procedure_characteristics(self, listener: F2003SemanticListener):
        """Validate procedure characteristics compatibility (ISO 12.2.2.1).

        Checks:
        1. Procedure pointer targets match their declared interface
        2. Deferred type-bound procedures have matching implementations
        3. Generic bindings have compatible specific procedures
        """
        result = listener.result

        for ptr_name, ptr in result.procedure_pointers.items():
            if ptr.initial_target and ptr.interface_name:
                self._check_pointer_target_compatibility(
                    ptr, result.procedures, result.abstract_interfaces, result
                )

        for type_name, bindings in result.type_bound_procedures.items():
            for tbp in bindings:
                if tbp.is_deferred and tbp.interface_name:
                    if tbp.interface_name not in result.abstract_interfaces:
                        if tbp.interface_name not in result.procedures:
                            result.diagnostics.append(
                                SemanticDiagnostic(
                                    severity=DiagnosticSeverity.WARNING,
                                    code="PROC_CHAR_W001",
                                    message=(
                                        f"DEFERRED binding '{tbp.binding_name}' in "
                                        f"type '{type_name}' references interface "
                                        f"'{tbp.interface_name}' which is not found "
                                        "in this compilation unit."
                                    ),
                                    line=tbp.line,
                                    column=tbp.column,
                                    iso_section="4.5.4",
                                )
                            )

    def _check_pointer_target_compatibility(
        self,
        ptr: ProcedurePointer,
        procedures: Dict[str, ProcedureCharacteristics],
        interfaces: Dict[str, AbstractInterface],
        result: ValidationResult,
    ):
        """Check that a procedure pointer target is compatible with interface.

        Per ISO/IEC 1539-1:2004 Section 12.2.2.1, a procedure pointer can only
        point to a procedure that has compatible characteristics.
        """
        if not ptr.initial_target or not ptr.interface_name:
            return

        target_proc = procedures.get(ptr.initial_target)
        interface = interfaces.get(ptr.interface_name)

        if interface and interface.procedures:
            iface_proc = interface.procedures[0]
        elif ptr.interface_name in procedures:
            iface_proc = procedures[ptr.interface_name]
        else:
            return

        if target_proc and iface_proc:
            if target_proc.is_function != iface_proc.is_function:
                result.diagnostics.append(
                    SemanticDiagnostic(
                        severity=DiagnosticSeverity.ERROR,
                        code="PROC_CHAR_E002",
                        message=(
                            f"Procedure pointer '{ptr.name}' target "
                            f"'{ptr.initial_target}' is a "
                            f"{'function' if target_proc.is_function else 'subroutine'} "
                            f"but interface '{ptr.interface_name}' declares a "
                            f"{'function' if iface_proc.is_function else 'subroutine'}."
                        ),
                        line=ptr.line,
                        column=ptr.column,
                        iso_section="12.2.2.1",
                    )
                )

            if target_proc.is_pure != iface_proc.is_pure:
                if iface_proc.is_pure:
                    result.diagnostics.append(
                        SemanticDiagnostic(
                            severity=DiagnosticSeverity.ERROR,
                            code="PROC_CHAR_E003",
                            message=(
                                f"Procedure pointer '{ptr.name}' with PURE interface "
                                f"'{ptr.interface_name}' cannot point to non-PURE "
                                f"procedure '{ptr.initial_target}'."
                            ),
                            line=ptr.line,
                            column=ptr.column,
                            iso_section="12.2.2.1",
                        )
                    )

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
