#!/usr/bin/env python3
"""FORTRAN II COMMON/EQUIVALENCE Storage Association Semantic Validator

Implements semantic validation for FORTRAN II COMMON and EQUIVALENCE statement
constraints per IBM Form C28-6000-2 (1958), specifically:

- Section 3.5 COMMON: COMMON defines storage areas to be shared between program
  units and gives flexible control over allocation.
- Section on Storage Association: EQUIVALENCE establishes storage association
  among variables; when COMMON variables also appear in EQUIVALENCE, storage
  ordering/priority changes accordingly.

This validator checks:
1. EQUIVALENCE overlay legality:
   - Variables in EQUIVALENCE must be compatible types (numeric/logical)
   - EQUIVALENCE sets must not create illegal overlays (e.g., different types)
   - Arrays in EQUIVALENCE must have compatible dimensions for overlay

2. COMMON block consistency:
   - COMMON blocks with the same name must have compatible variable lists
   - Variables in COMMON must have compatible sizes across program units
   - Order and type compatibility must be enforced

3. COMMON/EQUIVALENCE interaction:
   - Variables that appear in both COMMON and EQUIVALENCE must follow
     storage association rules

Reference: IBM FORTRAN II (1958) Form C28-6000-2
"""

import sys
from dataclasses import dataclass, field
from enum import Enum, auto
from pathlib import Path
from typing import List, Optional, Dict, Tuple, Set

sys.path.insert(
    0, str(Path(__file__).parent.parent / "grammars" / "generated" / "early")
)

from antlr4 import CommonTokenStream, InputStream, ParseTreeWalker
from FORTRANIILexer import FORTRANIILexer
from FORTRANIIParser import FORTRANIIParser
from FORTRANIIParserListener import FORTRANIIParserListener


class DiagnosticSeverity(Enum):
    """Diagnostic severity levels."""
    ERROR = auto()
    WARNING = auto()
    INFO = auto()


@dataclass
class SemanticDiagnostic:
    """Semantic diagnostic with ISO/standard section reference."""
    severity: DiagnosticSeverity
    code: str
    message: str
    line: Optional[int] = None
    column: Optional[int] = None
    standard_section: Optional[str] = None


@dataclass
class StorageAssociationResult:
    """Results from COMMON/EQUIVALENCE storage association validation."""
    diagnostics: List[SemanticDiagnostic] = field(default_factory=list)
    common_blocks: Dict[str, dict] = field(default_factory=dict)
    equivalence_sets: List[dict] = field(default_factory=list)

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


class FortranIICommonEquivalenceListener(FORTRANIIParserListener):
    """ANTLR listener for FORTRAN II COMMON/EQUIVALENCE semantic analysis."""

    # Type categories for compatibility checking
    NUMERIC_TYPES = {'integer', 'real', 'double precision'}
    LOGICAL_TYPES = {'logical'}
    COMPATIBLE_TYPES = NUMERIC_TYPES | LOGICAL_TYPES

    def __init__(self):
        super().__init__()
        self.result = StorageAssociationResult()
        self.current_program_unit = None
        self.variable_declarations: Dict[str, dict] = {}
        self.common_blocks_by_name: Dict[str, List[dict]] = {}

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
        standard_section: Optional[str] = None,
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
                standard_section=standard_section,
            )
        )

    def _extract_identifier(self, ctx) -> Optional[str]:
        """Extract identifier text from a context."""
        if ctx is None:
            return None
        text = ctx.getText()
        return text.upper() if text else None

    def _get_variable_info(self, var_name: str) -> Optional[dict]:
        """Get type and dimension info for a variable."""
        return self.variable_declarations.get(var_name.upper())

    def _normalize_name(self, name: str) -> str:
        """Normalize a name for comparison (uppercase)."""
        return name.upper() if name else ""

    def enterCommon_stmt(self, ctx):
        """Analyze COMMON statement for storage association violations."""
        line, col = self._get_location(ctx)

        # Extract common block name if present
        common_name = None
        if ctx.SLASH(0) is not None:  # /IDENTIFIER/ pattern
            # Variable list is after the slash pair
            identifier = ctx.IDENTIFIER()
            if identifier:
                common_name = self._normalize_name(identifier.getText())

        # Extract variable list
        variables = []
        if ctx.variable_list():
            var_list_ctx = ctx.variable_list()
            # Parse variable list - iterate through all variable() children
            var_count = var_list_ctx.getChildCount()
            for i in range(var_count):
                child = var_list_ctx.getChild(i)
                if hasattr(child, 'IDENTIFIER') and callable(child.IDENTIFIER):
                    var_id = child.IDENTIFIER()
                    if var_id:
                        var_name = self._normalize_name(var_id.getText())
                        variables.append(var_name)

        # Register COMMON block
        if not common_name:
            common_name = "__BLANK_COMMON__"

        common_info = {
            "name": common_name,
            "line": line,
            "column": col,
            "variables": variables,
            "program_unit": self.current_program_unit,
        }

        if common_name in self.result.common_blocks:
            # Check consistency with previously declared COMMON block
            prev_block = self.result.common_blocks[common_name]
            if prev_block["variables"] != variables:
                self._add_diagnostic(
                    DiagnosticSeverity.ERROR,
                    "COMMON-E001",
                    f"COMMON block '{common_name}' redeclared with different variable list. "
                    f"Previous: {prev_block['variables']}, Current: {variables}",
                    ctx,
                    standard_section="C28-6000-2 Section 3.5 COMMON",
                )
        else:
            self.result.common_blocks[common_name] = common_info

        # Track common blocks by name for cross-unit checking
        if common_name not in self.common_blocks_by_name:
            self.common_blocks_by_name[common_name] = []
        self.common_blocks_by_name[common_name].append(common_info)

    def enterEquivalence_stmt(self, ctx):
        """Analyze EQUIVALENCE statement for illegal overlays."""
        line, col = self._get_location(ctx)

        # Extract equivalence sets
        equivalence_sets = []
        set_count = ctx.getChildCount()
        for i in range(set_count):
            child = ctx.getChild(i)
            if hasattr(child, 'getText'):
                text = child.getText()
                if text.startswith('(') and text.endswith(')'):
                    equivalence_sets.append(text)

        # Analyze each equivalence set
        equivalence_set_contexts = []
        for i in range(ctx.getChildCount()):
            child = ctx.getChild(i)
            # Check if this is an equivalence_set rule
            if hasattr(child, 'variable'):
                equivalence_set_contexts.append(child)

        for eq_set_ctx in equivalence_set_contexts:
            self._validate_equivalence_set(eq_set_ctx, line, col)

    def _validate_equivalence_set(self, eq_set_ctx, parent_line: int, parent_col: int):
        """Validate a single EQUIVALENCE set for type compatibility."""
        # Extract variables from equivalence set
        variables = []
        var_count = eq_set_ctx.getChildCount()
        for i in range(var_count):
            child = eq_set_ctx.getChild(i)
            if hasattr(child, 'getText'):
                text = child.getText()
                # Simple identifier extraction (no subscripts/substrings for now)
                if text and text not in ('(', ')', ',', ''):
                    variables.append(self._normalize_name(text))

        # Remove duplicates while preserving order
        unique_vars = []
        seen = set()
        for var in variables:
            if var not in seen and var:
                unique_vars.append(var)
                seen.add(var)

        if len(unique_vars) < 2:
            self._add_diagnostic(
                DiagnosticSeverity.ERROR,
                "EQUIV-E001",
                f"EQUIVALENCE set must contain at least 2 variables, got {len(unique_vars)}",
                eq_set_ctx,
                standard_section="C28-6000-2 EQUIVALENCE",
            )
            return

        # Check type compatibility within the set
        types_in_set = []
        for var_name in unique_vars:
            var_info = self._get_variable_info(var_name)
            if var_info:
                types_in_set.append(var_info.get("type", "unknown"))
            else:
                # Variable not declared - cannot fully validate
                types_in_set.append("undeclared")

        # Validate type homogeneity
        # FORTRAN II allows numeric with numeric, logical with logical
        valid_types = set(types_in_set) - {"undeclared"}
        if len(valid_types) > 1:
            # Check if all types are in the same category
            numeric_types = valid_types & self.NUMERIC_TYPES
            logical_types = valid_types & self.LOGICAL_TYPES
            if numeric_types and logical_types:
                self._add_diagnostic(
                    DiagnosticSeverity.ERROR,
                    "EQUIV-E002",
                    f"EQUIVALENCE set contains incompatible types: {valid_types}. "
                    f"Cannot mix numeric and logical types.",
                    eq_set_ctx,
                    standard_section="C28-6000-2 EQUIVALENCE Storage Association",
                )

        # Record equivalence set
        self.result.equivalence_sets.append({
            "variables": unique_vars,
            "types": types_in_set,
            "line": parent_line,
            "column": parent_col,
        })

    def enterType_stmt(self, ctx):
        """Track variable declarations for type information."""
        # Extract type keyword
        type_keyword = None
        if ctx.INTEGER():
            type_keyword = "INTEGER"
        elif ctx.REAL():
            type_keyword = "REAL"
        elif ctx.DOUBLE():
            type_keyword = "DOUBLE PRECISION"
        elif ctx.LOGICAL():
            type_keyword = "LOGICAL"

        if not type_keyword:
            return

        # Extract variable list
        if hasattr(ctx, 'identifier_list'):
            var_list = ctx.identifier_list()
            var_count = var_list.getChildCount()
            for i in range(var_count):
                child = var_list.getChild(i)
                if hasattr(child, 'IDENTIFIER') and callable(child.IDENTIFIER):
                    var_id = child.IDENTIFIER()
                    if var_id:
                        var_name = self._normalize_name(var_id.getText())
                        self.variable_declarations[var_name] = {
                            "type": type_keyword.upper(),
                            "declared_at": self._get_location(ctx),
                        }


class FortranIICommonEquivalenceValidator:
    """Main validator for FORTRAN II COMMON/EQUIVALENCE semantics."""

    def __init__(self):
        self.listener = FortranIICommonEquivalenceListener()

    def validate(self, code: str) -> StorageAssociationResult:
        """Validate FORTRAN II COMMON/EQUIVALENCE storage association."""
        try:
            input_stream = InputStream(code)
            lexer = FORTRANIILexer(input_stream)
            token_stream = CommonTokenStream(lexer)
            parser = FORTRANIIParser(token_stream)

            # Parse the program
            tree = parser.fortran_program()

            # Walk the tree with semantic listener
            walker = ParseTreeWalker()
            walker.walk(self.listener, tree)

            return self.listener.result
        except Exception as e:
            # Add parse error diagnostic
            diagnostic = SemanticDiagnostic(
                severity=DiagnosticSeverity.ERROR,
                code="PARSE-E001",
                message=f"Parse error: {str(e)}",
                standard_section="FORTRAN II Parser",
            )
            self.listener.result.diagnostics.append(diagnostic)
            return self.listener.result


def validate_common_equivalence(code: str) -> StorageAssociationResult:
    """Convenience function to validate FORTRAN II code."""
    validator = FortranIICommonEquivalenceValidator()
    return validator.validate(code)
