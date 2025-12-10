#!/usr/bin/env python3
"""Fortran 2008 Submodule Semantic Validator

Implements semantic validation for Fortran 2008 submodule and separate module
procedure features per ISO/IEC 1539-1:2010 (Fortran 2008 standard).

This validator checks:
- Submodule declarations (Section 11.2.3)
- Parent module/submodule existence and hierarchy (R1118)
- Separate module procedures (Section 12.6.2.5)
- MODULE SUBROUTINE/FUNCTION linkage to parent interfaces

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
class ModuleDeclaration:
    """Represents a module declaration for semantic analysis."""

    name: str
    has_contains: bool = False
    interface_procedures: Set[str] = field(default_factory=set)
    public_procedures: Set[str] = field(default_factory=set)
    line: Optional[int] = None
    column: Optional[int] = None


@dataclass
class SubmoduleDeclaration:
    """Represents a submodule declaration for semantic analysis."""

    name: str
    parent_module: str
    parent_submodule: Optional[str] = None
    parent_chain: List[str] = field(default_factory=list)
    module_procedures: Set[str] = field(default_factory=set)
    line: Optional[int] = None
    column: Optional[int] = None


@dataclass
class ModuleProcedure:
    """Represents a MODULE SUBROUTINE/FUNCTION in a submodule."""

    name: str
    is_function: bool = False
    is_subroutine: bool = False
    submodule_name: Optional[str] = None
    has_result: bool = False
    result_name: Optional[str] = None
    dummy_args: List[str] = field(default_factory=list)
    line: Optional[int] = None
    column: Optional[int] = None


@dataclass
class InterfaceProcedure:
    """Represents a procedure interface in a module."""

    name: str
    is_function: bool = False
    is_subroutine: bool = False
    module_name: Optional[str] = None
    has_result: bool = False
    result_name: Optional[str] = None
    dummy_args: List[str] = field(default_factory=list)
    line: Optional[int] = None
    column: Optional[int] = None


@dataclass
class SubmoduleValidationResult:
    """Results from submodule semantic validation."""

    diagnostics: List[SemanticDiagnostic] = field(default_factory=list)
    modules: Dict[str, ModuleDeclaration] = field(default_factory=dict)
    submodules: Dict[str, SubmoduleDeclaration] = field(default_factory=dict)
    module_procedures: List[ModuleProcedure] = field(default_factory=list)
    interface_procedures: Dict[str, InterfaceProcedure] = field(default_factory=dict)

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


class F2008SubmoduleListener(Fortran2008ParserListener):
    """ANTLR listener for F2008 submodule semantic analysis."""

    def __init__(self):
        super().__init__()
        self.result = SubmoduleValidationResult()
        self._current_module: Optional[str] = None
        self._current_submodule: Optional[str] = None
        self._in_interface_block = False
        self._in_contains = False
        self._current_interface_name: Optional[str] = None

    def _get_token_text(self, ctx) -> str:
        if ctx is None:
            return ""
        return ctx.getText()

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

    def enterModule_stmt(self, ctx):
        """Track module declaration."""
        text = self._get_token_text(ctx)
        name_match = re.search(r"module\s*(\w+)", text, re.IGNORECASE)
        if name_match:
            name = name_match.group(1).lower()
            self._current_module = name
            line, col = self._get_location(ctx)
            self.result.modules[name] = ModuleDeclaration(
                name=name, line=line, column=col
            )

    def exitEnd_module_stmt(self, ctx):
        """Reset module context."""
        self._current_module = None
        self._in_contains = False

    def enterModule_subprogram_part(self, ctx):
        """Track entering CONTAINS section."""
        self._in_contains = True
        if self._current_module and self._current_module in self.result.modules:
            self.result.modules[self._current_module].has_contains = True

    def enterInterface_block(self, ctx):
        """Track interface block entry."""
        self._in_interface_block = True

    def exitInterface_block(self, ctx):
        """Track interface block exit."""
        self._in_interface_block = False
        self._current_interface_name = None

    def enterInterface_stmt(self, ctx):
        """Track interface statement - captures generic or abstract interface name."""
        text = self._get_token_text(ctx)
        name_match = re.search(r"interface\s*(\w+)", text, re.IGNORECASE)
        if name_match:
            self._current_interface_name = name_match.group(1).lower()

    def enterFunction_stmt_f2008(self, ctx):
        """Track function declarations in interface blocks and module procedures."""
        if self._in_interface_block and self._current_module:
            self._record_interface_function(ctx)
        elif self._has_module_prefix(ctx):
            self._record_module_subprogram(ctx, is_function=True)

    def enterFunction_stmt_interface(self, ctx):
        """Track function declarations in interface body."""
        if self._current_module:
            self._record_interface_function(ctx)

    def _record_interface_function(self, ctx):
        """Record an interface function declaration."""
        text = self._get_token_text(ctx)
        name_match = re.search(r"function\s*(\w+)", text, re.IGNORECASE)
        if name_match:
            name = name_match.group(1).lower()
            line, col = self._get_location(ctx)

            dummy_args = self._extract_dummy_args(text)
            result_name = self._extract_result_name(text)

            proc = InterfaceProcedure(
                name=name,
                is_function=True,
                is_subroutine=False,
                module_name=self._current_module,
                has_result=result_name is not None,
                result_name=result_name,
                dummy_args=dummy_args,
                line=line,
                column=col,
            )
            self.result.interface_procedures[name] = proc

            if self._current_module in self.result.modules:
                self.result.modules[self._current_module].interface_procedures.add(
                    name
                )

    def enterSubroutine_stmt_f2008(self, ctx):
        """Track subroutine declarations in interface blocks and module procedures."""
        if self._in_interface_block and self._current_module:
            self._record_interface_subroutine(ctx)
        elif self._has_module_prefix(ctx):
            self._record_module_subprogram(ctx, is_function=False)

    def enterSubroutine_stmt_interface(self, ctx):
        """Track subroutine declarations in interface body."""
        if self._current_module:
            self._record_interface_subroutine(ctx)

    def _record_interface_subroutine(self, ctx):
        """Record an interface subroutine declaration."""
        text = self._get_token_text(ctx)
        name_match = re.search(r"subroutine\s*(\w+)", text, re.IGNORECASE)
        if name_match:
            name = name_match.group(1).lower()
            line, col = self._get_location(ctx)

            dummy_args = self._extract_dummy_args(text)

            proc = InterfaceProcedure(
                name=name,
                is_function=False,
                is_subroutine=True,
                module_name=self._current_module,
                dummy_args=dummy_args,
                line=line,
                column=col,
            )
            self.result.interface_procedures[name] = proc

            if self._current_module in self.result.modules:
                self.result.modules[self._current_module].interface_procedures.add(
                    name
                )

    def enterSubmodule_stmt(self, ctx):
        """Track submodule declaration and parent hierarchy."""
        text = self._get_token_text(ctx)
        line, col = self._get_location(ctx)

        parent_match = re.search(r"submodule\s*\(\s*([^)]+)\s*\)\s*(\w+)", text, re.I)
        if parent_match:
            parent_spec = parent_match.group(1).strip()
            submod_name = parent_match.group(2).lower()

            parent_parts = [p.strip().lower() for p in parent_spec.split(":")]
            parent_module = parent_parts[0]
            parent_submodule = parent_parts[-1] if len(parent_parts) > 1 else None

            self._current_submodule = submod_name

            decl = SubmoduleDeclaration(
                name=submod_name,
                parent_module=parent_module,
                parent_submodule=parent_submodule,
                parent_chain=parent_parts,
                line=line,
                column=col,
            )
            self.result.submodules[submod_name] = decl

    def exitEnd_submodule_stmt(self, ctx):
        """Reset submodule context."""
        self._current_submodule = None
        self._in_contains = False

    def enterModule_subroutine_stmt_f2008(self, ctx):
        """Track MODULE SUBROUTINE in submodule."""
        self._record_module_subprogram(ctx, is_function=False)

    def enterModule_function_stmt_f2008(self, ctx):
        """Track MODULE FUNCTION in submodule."""
        self._record_module_subprogram(ctx, is_function=True)

    def _extract_dummy_args(self, text: str) -> List[str]:
        """Extract dummy argument names from procedure statement."""
        args_match = re.search(r"\(([^)]*)\)", text)
        if args_match:
            args_str = args_match.group(1)
            args = [
                a.strip().lower() for a in args_str.split(",") if a.strip()
            ]
            return [a for a in args if a and not a.startswith("result")]
        return []

    def _extract_result_name(self, text: str) -> Optional[str]:
        """Extract RESULT clause name from function statement."""
        result_match = re.search(r"result\s*\(\s*(\w+)\s*\)", text, re.IGNORECASE)
        if result_match:
            return result_match.group(1).lower()
        return None

    def _has_module_prefix(self, ctx) -> bool:
        """Return True if the statement context has MODULE in its prefix spec."""
        if ctx is None:
            return False
        prefix = ctx.prefix_f2008()
        if not prefix:
            return False
        for spec in prefix.prefix_spec_f2008():
            if spec.MODULE():
                return True
        return False

    def _record_module_subprogram(self, ctx, is_function: bool):
        """Record a MODULE FUNCTION/SUBROUTINE from any applicable statement."""
        text = self._get_token_text(ctx)
        line, col = self._get_location(ctx)
        pattern = r"function\s*(\w+)" if is_function else r"subroutine\s*(\w+)"
        name_match = re.search(pattern, text, re.IGNORECASE)
        if not name_match:
            return

        name = name_match.group(1).lower()
        dummy_args = self._extract_dummy_args(text)
        result_name = self._extract_result_name(text) if is_function else None

        proc = ModuleProcedure(
            name=name,
            is_function=is_function,
            is_subroutine=not is_function,
            submodule_name=self._current_submodule,
            has_result=result_name is not None,
            result_name=result_name,
            dummy_args=dummy_args,
            line=line,
            column=col,
        )
        self.result.module_procedures.append(proc)

        if (
            self._current_submodule
            and self._current_submodule in self.result.submodules
        ):
            self.result.submodules[self._current_submodule].module_procedures.add(
                name
            )


class F2008SubmoduleValidator:
    """Semantic validator for Fortran 2008 submodule features."""

    def __init__(self):
        self._lexer = None
        self._parser = None

    def validate_code(self, code: str) -> SubmoduleValidationResult:
        """Validate Fortran 2008 code for submodule semantics."""
        input_stream = InputStream(code)
        self._lexer = Fortran2008Lexer(input_stream)
        token_stream = CommonTokenStream(self._lexer)
        self._parser = Fortran2008Parser(token_stream)

        tree = self._parser.program_unit_f2008()

        syntax_errors = self._parser.getNumberOfSyntaxErrors()
        result = SubmoduleValidationResult()

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

        listener = F2008SubmoduleListener()
        walker = ParseTreeWalker()
        walker.walk(listener, tree)

        self._perform_semantic_validation(listener.result)

        return listener.result

    def validate_multi_unit(self, code_units: List[str]) -> SubmoduleValidationResult:
        """Validate multiple Fortran 2008 program units for submodule linkage.

        This method validates cross-unit semantic constraints:
        - Parent module existence for submodules
        - Parent submodule hierarchy consistency
        - Module procedure interface linkage

        Per ISO/IEC 1539-1:2010 Section 11.2.3, a submodule must have
        a valid parent module or submodule.
        """
        combined_result = SubmoduleValidationResult()

        for code in code_units:
            unit_result = self._parse_single_unit(code)
            if unit_result.has_errors:
                combined_result.diagnostics.extend(unit_result.diagnostics)
                continue

            for name, mod in unit_result.modules.items():
                combined_result.modules[name] = mod
            for name, submod in unit_result.submodules.items():
                combined_result.submodules[name] = submod
            combined_result.module_procedures.extend(unit_result.module_procedures)
            for name, proc in unit_result.interface_procedures.items():
                combined_result.interface_procedures[name] = proc

        self._perform_cross_unit_validation(combined_result)

        return combined_result

    def _parse_single_unit(self, code: str) -> SubmoduleValidationResult:
        """Parse a single program unit without cross-validation."""
        input_stream = InputStream(code)
        lexer = Fortran2008Lexer(input_stream)
        token_stream = CommonTokenStream(lexer)
        parser = Fortran2008Parser(token_stream)

        tree = parser.program_unit_f2008()

        syntax_errors = parser.getNumberOfSyntaxErrors()
        result = SubmoduleValidationResult()

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

        listener = F2008SubmoduleListener()
        walker = ParseTreeWalker()
        walker.walk(listener, tree)

        return listener.result

    def _perform_semantic_validation(self, result: SubmoduleValidationResult):
        """Perform single-unit semantic validation."""
        self._validate_submodule_declarations(result)
        self._validate_module_procedures(result)

    def _perform_cross_unit_validation(self, result: SubmoduleValidationResult):
        """Perform cross-unit semantic validation for submodule linkage."""
        self._validate_parent_existence(result)
        self._validate_parent_hierarchy(result)
        self._validate_procedure_linkage(result)
        self._check_duplicate_implementations(result)

    def _validate_submodule_declarations(self, result: SubmoduleValidationResult):
        """Validate individual submodule declarations."""
        for name, submod in result.submodules.items():
            if not submod.parent_module:
                result.diagnostics.append(
                    SemanticDiagnostic(
                        severity=DiagnosticSeverity.ERROR,
                        code="SUBMOD_E001",
                        message=f"Submodule '{name}' has no parent module specified. "
                        "Per ISO/IEC 1539-1:2010 Section 11.2.3, R1117, "
                        "a submodule-stmt must include a parent-identifier.",
                        line=submod.line,
                        column=submod.column,
                        iso_section="11.2.3",
                    )
                )

    def _validate_module_procedures(self, result: SubmoduleValidationResult):
        """Validate module procedure declarations within submodules."""
        for proc in result.module_procedures:
            if not proc.submodule_name:
                result.diagnostics.append(
                    SemanticDiagnostic(
                        severity=DiagnosticSeverity.INFO,
                        code="SUBMOD_I001",
                        message=f"MODULE {proc_type(proc)} '{proc.name}' found "
                        "outside of a submodule context. Per ISO/IEC 1539-1:2010 "
                        "Section 12.6.2.5, separate module subprograms are typically "
                        "defined in submodules.",
                        line=proc.line,
                        column=proc.column,
                        iso_section="12.6.2.5",
                    )
                )

    def _validate_parent_existence(self, result: SubmoduleValidationResult):
        """Validate that parent modules/submodules exist."""
        for name, submod in result.submodules.items():
            parent_mod = submod.parent_module
            if parent_mod not in result.modules:
                result.diagnostics.append(
                    SemanticDiagnostic(
                        severity=DiagnosticSeverity.WARNING,
                        code="SUBMOD_W001",
                        message=f"Submodule '{name}' references parent module "
                        f"'{parent_mod}' which is not defined in the validated "
                        "code units. Per ISO/IEC 1539-1:2010 Section 11.2.3, "
                        "the ancestor module must exist.",
                        line=submod.line,
                        column=submod.column,
                        iso_section="11.2.3",
                    )
                )

            if submod.parent_submodule:
                parent_sub = submod.parent_submodule
                if parent_sub not in result.submodules:
                    result.diagnostics.append(
                        SemanticDiagnostic(
                            severity=DiagnosticSeverity.WARNING,
                            code="SUBMOD_W002",
                            message=f"Submodule '{name}' references parent "
                            f"submodule '{parent_sub}' which is not defined in "
                            "the validated code units. Per ISO/IEC 1539-1:2010 "
                            "R1118, the parent-submodule-name must refer to an "
                            "existing submodule.",
                            line=submod.line,
                            column=submod.column,
                            iso_section="11.2.3",
                        )
                    )

    def _validate_parent_hierarchy(self, result: SubmoduleValidationResult):
        """Validate submodule hierarchy consistency."""
        visited: Set[str] = set()
        for name, submod in result.submodules.items():
            if name in visited:
                continue
            chain = self._build_ancestor_chain(name, result.submodules, visited)
            if chain is None:
                result.diagnostics.append(
                    SemanticDiagnostic(
                        severity=DiagnosticSeverity.ERROR,
                        code="SUBMOD_E002",
                        message=f"Circular dependency detected in submodule "
                        f"hierarchy involving '{name}'. Per ISO/IEC 1539-1:2010 "
                        "Section 11.2.3, submodule hierarchies must be acyclic.",
                        line=submod.line,
                        column=submod.column,
                        iso_section="11.2.3",
                    )
                )

    def _build_ancestor_chain(
        self,
        name: str,
        submodules: Dict[str, SubmoduleDeclaration],
        visited: Set[str],
    ) -> Optional[List[str]]:
        """Build ancestor chain, returning None if cycle detected."""
        chain: List[str] = []
        current = name
        path: Set[str] = set()

        while current:
            if current in path:
                return None
            path.add(current)
            chain.append(current)
            visited.add(current)

            if current not in submodules:
                break

            submod = submodules[current]
            current = submod.parent_submodule

        return chain

    def _validate_procedure_linkage(self, result: SubmoduleValidationResult):
        """Validate that module procedures link to parent interfaces."""
        for proc in result.module_procedures:
            proc_name = proc.name

            parent_module: Optional[str] = None
            if proc.submodule_name and proc.submodule_name in result.submodules:
                parent_module = result.submodules[proc.submodule_name].parent_module

            if parent_module:
                candidates = [
                    iface
                    for iface in result.interface_procedures.values()
                    if iface.name == proc_name and iface.module_name == parent_module
                ]
            else:
                candidates = [
                    iface
                    for iface in result.interface_procedures.values()
                    if iface.name == proc_name
                ]

            if candidates:
                self._check_signature_match(proc, candidates[0], result)
            else:
                result.diagnostics.append(
                    SemanticDiagnostic(
                        severity=DiagnosticSeverity.INFO,
                        code="SUBMOD_I002",
                        message=f"MODULE {proc_type(proc)} '{proc_name}' has no "
                        "matching interface in the validated module scope. "
                        "Per ISO/IEC 1539-1:2010 Section 12.6.2.5, a separate "
                        "module subprogram shall have an interface in its "
                        "ancestor module.",
                        line=proc.line,
                        column=proc.column,
                        iso_section="12.6.2.5",
                    )
                )

    def _check_signature_match(
        self,
        impl: ModuleProcedure,
        iface: InterfaceProcedure,
        result: SubmoduleValidationResult,
    ):
        """Check that implementation signature matches interface."""
        if impl.is_function != iface.is_function:
            result.diagnostics.append(
                SemanticDiagnostic(
                    severity=DiagnosticSeverity.ERROR,
                    code="SUBMOD_E003",
                    message=f"MODULE {proc_type(impl)} '{impl.name}' type mismatch: "
                    f"implementation is a {proc_type(impl)} but interface "
                    f"declares a {proc_type_from_iface(iface)}. Per ISO/IEC "
                    "1539-1:2010 Section 12.6.2.5, the separate module "
                    "subprogram must match its interface.",
                    line=impl.line,
                    column=impl.column,
                    iso_section="12.6.2.5",
                )
            )

        if len(impl.dummy_args) != len(iface.dummy_args):
            result.diagnostics.append(
                SemanticDiagnostic(
                    severity=DiagnosticSeverity.WARNING,
                    code="SUBMOD_W003",
                    message=f"MODULE {proc_type(impl)} '{impl.name}' has "
                    f"{len(impl.dummy_args)} dummy arguments but interface "
                    f"declares {len(iface.dummy_args)}. Per ISO/IEC 1539-1:2010 "
                    "Section 12.6.2.5, the characteristics must match.",
                    line=impl.line,
                    column=impl.column,
                    iso_section="12.6.2.5",
                )
            )

    def _check_duplicate_implementations(self, result: SubmoduleValidationResult):
        """Check for duplicate module procedure implementations."""
        impl_counts: Dict[str, List[ModuleProcedure]] = {}
        for proc in result.module_procedures:
            if proc.name not in impl_counts:
                impl_counts[proc.name] = []
            impl_counts[proc.name].append(proc)

        for name, impls in impl_counts.items():
            if len(impls) > 1:
                locations = ", ".join(
                    f"line {p.line}" for p in impls if p.line is not None
                )
                result.diagnostics.append(
                    SemanticDiagnostic(
                        severity=DiagnosticSeverity.ERROR,
                        code="SUBMOD_E004",
                        message=f"Multiple implementations found for MODULE "
                        f"procedure '{name}' at {locations}. Per ISO/IEC "
                        "1539-1:2010 Section 12.6.2.5, each separate module "
                        "subprogram shall have exactly one definition.",
                        line=impls[0].line,
                        column=impls[0].column,
                        iso_section="12.6.2.5",
                    )
                )

    def validate_file(self, filepath: str) -> SubmoduleValidationResult:
        """Validate a Fortran 2008 file for submodule semantics."""
        path = Path(filepath)
        if not path.exists():
            result = SubmoduleValidationResult()
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


def proc_type(proc: ModuleProcedure) -> str:
    """Return FUNCTION or SUBROUTINE based on procedure type."""
    return "FUNCTION" if proc.is_function else "SUBROUTINE"


def proc_type_from_iface(iface: InterfaceProcedure) -> str:
    """Return FUNCTION or SUBROUTINE based on interface type."""
    return "FUNCTION" if iface.is_function else "SUBROUTINE"


def validate_submodule_semantics(code: str) -> SubmoduleValidationResult:
    """Convenience function to validate submodule semantics."""
    validator = F2008SubmoduleValidator()
    return validator.validate_code(code)


def validate_submodule_linkage(code_units: List[str]) -> SubmoduleValidationResult:
    """Convenience function to validate submodule linkage across units."""
    validator = F2008SubmoduleValidator()
    return validator.validate_multi_unit(code_units)
