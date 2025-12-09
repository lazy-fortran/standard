#!/usr/bin/env python3

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
class ValidationResult:
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
    "c_int", "c_short", "c_long", "c_long_long",
    "c_signed_char", "c_size_t",
    "c_int8_t", "c_int16_t", "c_int32_t", "c_int64_t",
    "c_int_least8_t", "c_int_least16_t", "c_int_least32_t", "c_int_least64_t",
    "c_int_fast8_t", "c_int_fast16_t", "c_int_fast32_t", "c_int_fast64_t",
    "c_intmax_t", "c_intptr_t",
    # Floating-point types
    "c_float", "c_double", "c_long_double",
    "c_float_complex", "c_double_complex", "c_long_double_complex",
    # Other types
    "c_bool", "c_char",
    # Pointer types
    "c_ptr", "c_funptr",
}

# ISO/IEC 1539-1:2004 Section 15.2.3: C pointer constants
C_POINTER_CONSTANTS: Set[str] = {"c_null_ptr", "c_null_funptr"}

# ISO/IEC 1539-1:2004 Section 14: IEEE module entities
IEEE_MODULES: Set[str] = {"ieee_exceptions", "ieee_arithmetic", "ieee_features"}

# IEEE exception flags (Section 14.2)
IEEE_EXCEPTION_FLAGS: Set[str] = {
    "ieee_overflow", "ieee_underflow", "ieee_divide_by_zero",
    "ieee_invalid", "ieee_inexact",
}

# IEEE special values (Section 14.3)
IEEE_SPECIAL_VALUES: Set[str] = {
    "ieee_positive_inf", "ieee_negative_inf",
    "ieee_quiet_nan", "ieee_signaling_nan",
}

# IEEE rounding modes (Section 14.3)
IEEE_ROUNDING_MODES: Set[str] = {
    "ieee_nearest", "ieee_to_zero", "ieee_up", "ieee_down",
}

# IEEE features (Section 14.4)
IEEE_FEATURES: Set[str] = {
    "ieee_datatype", "ieee_denormal", "ieee_divide", "ieee_halting",
    "ieee_inexact_flag", "ieee_inf", "ieee_invalid_flag",
    "ieee_nan", "ieee_rounding", "ieee_sqrt", "ieee_underflow_flag",
}

# All IEEE entities for validation
ALL_IEEE_ENTITIES: Set[str] = (
    IEEE_EXCEPTION_FLAGS | IEEE_SPECIAL_VALUES | IEEE_ROUNDING_MODES | IEEE_FEATURES
)


# ISO/IEC 1539-1:2004 Section 12.3.2.2: Procedure characteristics
# Each procedure has characteristics that determine compatibility
@dataclass
class ArgumentCharacteristic:
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
    name: Optional[str] = None
    procedures: List[ProcedureCharacteristics] = field(default_factory=list)
    line: Optional[int] = None
    column: Optional[int] = None


@dataclass
class ProcedurePointer:
    name: str
    interface_name: Optional[str] = None
    is_component: bool = False
    initial_target: Optional[str] = None
    pass_attr: Optional[str] = None
    line: Optional[int] = None
    column: Optional[int] = None


@dataclass
class TypeBoundProcedure:
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
    generic_name: str
    specific_bindings: List[str] = field(default_factory=list)
    is_operator: bool = False
    access: Optional[str] = None
    line: Optional[int] = None
    column: Optional[int] = None

class F2003SemanticListener(Fortran2003ParserListener):
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

    def enterUse_stmt(self, ctx):
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
        text = self._get_token_text(ctx)
        if "bind" in text and "c" in text:
            self._in_bind_c_procedure = True

    def exitBinding_spec(self, ctx):
        pass

    def enterFunction_stmt(self, ctx):
        text = self._get_token_text(ctx)
        if "bind" in text:
            self._in_bind_c_procedure = True
            # Extract function name
            if hasattr(ctx, "identifier_or_keyword") and ctx.identifier_or_keyword:
                name = ctx.identifier_or_keyword().getText()
                self._current_procedure_name = name
                self.result.c_interop_entities.append(name)

    def exitFunction_stmt(self, ctx):
        pass

    def enterSubroutine_stmt(self, ctx):
        text = self._get_token_text(ctx)
        if "bind" in text:
            self._in_bind_c_procedure = True

    def enterSubroutine_stmt_f2003(self, ctx):
        text = self._get_token_text(ctx)
        if "bind" in text:
            self._in_bind_c_procedure = True
            # Try to extract subroutine name
            self._extract_procedure_name_from_text(text, ctx)

    def _extract_procedure_name_from_text(self, text: str, ctx):
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
        text = self._get_token_text(ctx)
        if "bind" in text:
            self._in_bind_c_procedure = True

    def enterType_attr_spec(self, ctx):
        text = self._get_token_text(ctx)
        if "bind" in text:
            self._in_bind_c_procedure = True

    def exitEnd_function_stmt(self, ctx):
        self._in_bind_c_procedure = False
        self._current_procedure_name = None

    def exitEnd_subroutine_stmt(self, ctx):
        self._in_bind_c_procedure = False
        self._current_procedure_name = None

    def exitEnd_type_stmt(self, ctx):
        self._in_bind_c_procedure = False

    def exitEnd_type_stmt_f2003(self, ctx):
        self._in_bind_c_procedure = False

    def enterAttr_spec(self, ctx):
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
        pass

    def exitInterface_body(self, ctx):
        if self._current_procedure and self._in_abstract_interface:
            if self._current_abstract_interface:
                self._current_abstract_interface.procedures.append(
                    self._current_procedure
                )
        self._current_procedure = None

    def enterFunction_stmt_f2003(self, ctx):
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
        match = re.search(r"function\s+(\w+)", text)
        if match:
            return match.group(1)
        return None

    def _extract_result_name(self, text: str) -> Optional[str]:
        match = re.search(r"result\s*\(\s*(\w+)\s*\)", text)
        if match:
            return match.group(1)
        return None

    def _extract_prefix_type(self, text: str) -> Optional[str]:
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
        if self._current_procedure:
            self.result.procedures[self._current_procedure.name] = (
                self._current_procedure
            )
        self._current_procedure = None
        self._in_bind_c_procedure = False
        self._current_procedure_name = None

    def enterSubroutine_stmt_interface(self, ctx):
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
        match = re.search(r"subroutine\s+(\w+)", text)
        if match:
            return match.group(1)
        return None

    def exitSubroutine_subprogram_f2003(self, ctx):
        if self._current_procedure:
            self.result.procedures[self._current_procedure.name] = (
                self._current_procedure
            )
        self._current_procedure = None
        self._in_bind_c_procedure = False
        self._current_procedure_name = None

    def enterProcedure_declaration_stmt(self, ctx):
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
        pattern = rf"{ptr_name}\s*=>\s*(\w+)"
        match = re.search(pattern, text)
        if match:
            target = match.group(1)
            if target != "null":
                return target
        return None

    def enterProc_component_def_stmt(self, ctx):
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
        text = self._get_token_text(ctx)
        line, col = self._get_location(ctx)

        type_match = re.search(r"type\s*(?:,.*?)?(?:::)?\s*(\w+)", text)
        if type_match:
            self._current_type_name = type_match.group(1)

        if "bind" in text:
            self._in_bind_c_procedure = True

    def enterType_bound_procedure_part(self, ctx):
        self._in_type_bound_part = True

    def exitType_bound_procedure_part(self, ctx):
        self._in_type_bound_part = False

    def enterType_bound_procedure_stmt(self, ctx):
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
        bindings = []
        arrow_pos = text.find("=>")
        if arrow_pos >= 0:
            bindings_part = text[arrow_pos + 2 :]
            binding_pattern = re.compile(r"(\w+)")
            for match in binding_pattern.finditer(bindings_part):
                bindings.append(match.group(1))
        return bindings

    def exitDerived_type_def_f2003(self, ctx):
        self._current_type_name = None
        self._in_bind_c_procedure = False


class F2003SemanticValidator:
    def __init__(self):
        self._lexer = None
        self._parser = None

    def validate_code(self, code: str) -> ValidationResult:
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

        self._validate_procedure_characteristics(listener)

        return listener.result

    def _validate_procedure_characteristics(self, listener: F2003SemanticListener):
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
    validator = F2003SemanticValidator()
    return validator.validate_code(code)


def validate_ieee_arithmetic(code: str) -> ValidationResult:
    validator = F2003SemanticValidator()
    return validator.validate_code(code)
