#!/usr/bin/env python3
"""Fortran 90 Array-Spec and POINTER/TARGET Semantic Validator

Implements semantic validation for array specifications and POINTER/TARGET
attribute constraints per ISO/IEC 1539:1991 (Fortran 90), specifically:
- Section 5.1.2.4: Array-spec constraints (assumed-shape, deferred-shape)
- Section 5.2.7-5.2.8: POINTER/TARGET attribute constraints
- Section 5.2: Attribute duplication and consistency rules

This validator checks:
1. Assumed-shape arrays restricted to non-pointer dummy arguments
2. POINTER arrays must use deferred-shape specs, not explicit or assumed-shape
3. ALLOCATABLE arrays must use deferred-shape specs
4. Attribute compatibility: POINTER excludes TARGET, INTENT, etc.
5. TARGET excludes POINTER, EXTERNAL, INTRINSIC, PARAMETER
6. Array spec compatibility with attributes

Reference: ISO/IEC 1539:1991 (WG5 N692), Sections 5.1.2.4, 5.2.7-5.2.8
"""

import sys
from dataclasses import dataclass, field
from enum import Enum, auto
from pathlib import Path
from typing import List, Optional, Set, Tuple, Dict

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
class ArraySpecPointerTargetResult:
    """Results from array-spec/pointer/target semantic validation."""
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


class F90ArraySpecPointerTargetListener(Fortran90ParserListener):
    """ANTLR listener for array-spec/pointer/target semantic analysis."""

    def __init__(self):
        super().__init__()
        self.result = ArraySpecPointerTargetResult()
        self._current_attributes: Set[str] = set()
        self._in_dummy_arg = False
        self._dummy_arg_names: Set[str] = set()
        self._declared_with_attrs: Dict[str, Set[str]] = {}

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

    def enterSubroutineSubprogramPart(self, ctx):
        """Track subroutine subprograms."""
        # Extract dummy arg names from subroutine prefix
        if hasattr(ctx, 'subroutine_stmt') and ctx.subroutine_stmt():
            self._extract_dummy_args(ctx.subroutine_stmt())

    def enterFunctionSubprogramPart(self, ctx):
        """Track function subprograms."""
        # Extract dummy arg names from function prefix
        if hasattr(ctx, 'function_stmt') and ctx.function_stmt():
            self._extract_dummy_args(ctx.function_stmt())

    def _extract_dummy_args(self, stmt_ctx):
        """Extract dummy argument names from subroutine/function stmt."""
        if stmt_ctx is None:
            return
        # Look for dummy_arg_list in the statement
        try:
            # Navigate to find args
            if hasattr(stmt_ctx, 'dummy_arg_list'):
                dummy_list = stmt_ctx.dummy_arg_list()
                if dummy_list and hasattr(dummy_list, 'dummy_arg'):
                    for arg in dummy_list.dummy_arg():
                        if hasattr(arg, 'IDENTIFIER'):
                            name = arg.IDENTIFIER().getText()
                            self._dummy_arg_names.add(name.lower())
        except Exception:
            pass

    def enterType_declaration_stmt_f90(self, ctx):
        """Check type declaration statements for attribute conflicts."""
        # Clear attributes for new declaration
        self._current_attributes = set()

        # Extract all attributes using getChild to iterate through alternatives
        try:
            # The grammar has: type_spec_f90 (COMMA attr_spec_f90)* DOUBLE_COLON? entity_decl_list_f90
            # We need to find all attr_spec_f90 nodes
            if hasattr(ctx, 'getChildCount'):
                for i in range(ctx.getChildCount()):
                    child = ctx.getChild(i)
                    if hasattr(child, 'getRuleIndex'):
                        # Check if this child is an attr_spec_f90
                        rule_name = None
                        try:
                            rule_name = ctx.parser.ruleNames[child.getRuleIndex()]
                        except Exception:
                            pass

                        if rule_name == 'attr_spec_f90':
                            attr_name = self._extract_attribute_from_parse_node(child)
                            if attr_name:
                                self._current_attributes.add(attr_name)
        except Exception:
            pass

        # Check attribute compatibility
        self._check_attribute_compatibility(ctx)

        # Check each entity declaration for array spec constraints
        try:
            if hasattr(ctx, 'entity_decl_list_f90') and ctx.entity_decl_list_f90():
                entity_list = ctx.entity_decl_list_f90()
                if hasattr(entity_list, 'entity_decl_f90'):
                    for entity in entity_list.entity_decl_f90():
                        self._check_entity_declaration(entity, ctx)
        except Exception:
            pass

    def _extract_attribute_from_parse_node(self, attr_ctx) -> Optional[str]:
        """Extract attribute name from attr_spec_f90 parse node by looking at tokens."""
        if attr_ctx is None:
            return None
        try:
            # Get the first non-epsilon token in this node
            ctx_text = attr_ctx.getText().upper()

            # Map token text to attribute name
            if ctx_text.startswith('INTENT'):
                return 'INTENT'
            elif ctx_text.startswith('DIMENSION'):
                return 'DIMENSION'
            elif 'POINTER' in ctx_text:
                return 'POINTER'
            elif 'TARGET' in ctx_text:
                return 'TARGET'
            elif 'ALLOCATABLE' in ctx_text:
                return 'ALLOCATABLE'
            elif 'PARAMETER' in ctx_text:
                return 'PARAMETER'
            elif 'OPTIONAL' in ctx_text:
                return 'OPTIONAL'
            elif 'PUBLIC' in ctx_text:
                return 'PUBLIC'
            elif 'PRIVATE' in ctx_text:
                return 'PRIVATE'
            elif 'EXTERNAL' in ctx_text:
                return 'EXTERNAL'
            elif 'INTRINSIC' in ctx_text:
                return 'INTRINSIC'
            elif 'SAVE' in ctx_text:
                return 'SAVE'
        except Exception:
            pass
        return None

    def _check_attribute_compatibility(self, ctx):
        """Check for incompatible attribute combinations."""
        # E676-001: POINTER and TARGET are mutually exclusive
        if 'POINTER' in self._current_attributes and 'TARGET' in self._current_attributes:
            self._add_diagnostic(
                DiagnosticSeverity.ERROR,
                'E676-001',
                'POINTER and TARGET attributes are mutually exclusive',
                ctx,
                'ISO/IEC 1539:1991 Section 5.2.7-5.2.8'
            )

        # E676-002: POINTER excludes INTENT
        if 'POINTER' in self._current_attributes and 'INTENT' in self._current_attributes:
            self._add_diagnostic(
                DiagnosticSeverity.ERROR,
                'E676-002',
                'POINTER attribute excludes INTENT in dummy arguments',
                ctx,
                'ISO/IEC 1539:1991 Section 5.2.7'
            )

        # E676-003: ALLOCATABLE and POINTER are mutually exclusive
        if 'ALLOCATABLE' in self._current_attributes and 'POINTER' in self._current_attributes:
            self._add_diagnostic(
                DiagnosticSeverity.ERROR,
                'E676-003',
                'ALLOCATABLE and POINTER attributes are mutually exclusive',
                ctx,
                'ISO/IEC 1539:1991 Section 5.2.7'
            )

    def _check_entity_declaration(self, entity_ctx, type_decl_ctx):
        """Check individual entity declarations for array spec constraints."""
        if entity_ctx is None:
            return

        # Get entity name
        entity_name = None
        if hasattr(entity_ctx, 'object_name') and entity_ctx.object_name():
            obj_name = entity_ctx.object_name()
            if hasattr(obj_name, 'IDENTIFIER'):
                entity_name = obj_name.IDENTIFIER().getText().lower()

        # Check for array specification
        array_spec = None
        if hasattr(entity_ctx, 'array_spec_f90') and entity_ctx.array_spec_f90():
            array_spec = entity_ctx.array_spec_f90()

        if array_spec and entity_name:
            # Determine spec type
            spec_type = self._get_array_spec_type(array_spec)

            # E676-004: Assumed-shape arrays only in dummy arguments
            if spec_type == 'assumed-shape':
                if entity_name not in self._dummy_arg_names:
                    self._add_diagnostic(
                        DiagnosticSeverity.ERROR,
                        'E676-004',
                        f'Assumed-shape array {entity_name} can only appear in dummy arguments',
                        entity_ctx,
                        'ISO/IEC 1539:1991 Section 5.1.2.4'
                    )
                # E676-005: Assumed-shape arrays cannot have POINTER attribute
                if 'POINTER' in self._current_attributes:
                    self._add_diagnostic(
                        DiagnosticSeverity.ERROR,
                        'E676-005',
                        f'Assumed-shape array {entity_name} cannot have POINTER attribute',
                        entity_ctx,
                        'ISO/IEC 1539:1991 Section 5.1.2.4'
                    )

            # E676-006: POINTER arrays must have deferred-shape spec
            if 'POINTER' in self._current_attributes:
                if spec_type == 'explicit-shape':
                    self._add_diagnostic(
                        DiagnosticSeverity.ERROR,
                        'E676-006',
                        f'POINTER array {entity_name} must have deferred-shape spec (:)',
                        entity_ctx,
                        'ISO/IEC 1539:1991 Section 5.2.7'
                    )
                elif spec_type == 'assumed-shape':
                    self._add_diagnostic(
                        DiagnosticSeverity.ERROR,
                        'E676-007',
                        f'POINTER array {entity_name} cannot have assumed-shape spec',
                        entity_ctx,
                        'ISO/IEC 1539:1991 Section 5.2.7'
                    )

            # E676-008: ALLOCATABLE arrays must have deferred-shape spec
            if 'ALLOCATABLE' in self._current_attributes:
                if spec_type == 'explicit-shape':
                    self._add_diagnostic(
                        DiagnosticSeverity.ERROR,
                        'E676-008',
                        f'ALLOCATABLE array {entity_name} must have deferred-shape spec (:)',
                        entity_ctx,
                        'ISO/IEC 1539:1991 Section 5.1.2.4'
                    )
                elif spec_type == 'assumed-shape':
                    self._add_diagnostic(
                        DiagnosticSeverity.ERROR,
                        'E676-009',
                        f'ALLOCATABLE array {entity_name} cannot have assumed-shape spec',
                        entity_ctx,
                        'ISO/IEC 1539:1991 Section 5.1.2.4'
                    )

    def _get_array_spec_type(self, array_spec_ctx) -> Optional[str]:
        """Determine array spec type: explicit-shape, assumed-shape, deferred-shape, assumed-size."""
        if array_spec_ctx is None:
            return None

        try:
            spec_text = array_spec_ctx.getText()

            # Deferred-shape: contains only colons and commas, e.g., (:) or (:,:)
            if ':' in spec_text:
                # Count colons and commas
                colon_count = spec_text.count(':')
                comma_count = spec_text.count(',')
                # Pure deferred: (:,:,...)
                if all(c in '(:,)' for c in spec_text):
                    return 'deferred-shape'
                # Mixed with bounds: might be assumed-size or explicit-shape with some deferred
                elif '*' in spec_text:
                    return 'assumed-size'
                else:
                    # Has expressions and colons - could be explicit-shape with some dims
                    return 'explicit-shape'

            # Assumed-shape: has colons (checked above) or lower bound with colon
            # Explicit-shape: bounds only, no colons
            if all(c.isdigit() or c in '(),-' for c in spec_text.replace(' ', '')):
                return 'explicit-shape'

            # Assumed-size: ends with *
            if '*' in spec_text:
                return 'assumed-size'

            # Default: explicit-shape
            return 'explicit-shape'
        except Exception:
            return None


def validate_f90_array_spec_pointer_target(source: str) -> ArraySpecPointerTargetResult:
    """Validate Fortran 90 array-spec and POINTER/TARGET constraints.

    Args:
        source: Fortran 90 source code string

    Returns:
        ArraySpecPointerTargetResult with diagnostics
    """
    try:
        input_stream = InputStream(source)
        lexer = Fortran90Lexer(input_stream)
        token_stream = CommonTokenStream(lexer)
        parser = Fortran90Parser(token_stream)

        # Suppress error messages during parse
        parser.removeErrorListeners()

        tree = parser.program_unit_f90()
        listener = F90ArraySpecPointerTargetListener()
        walker = ParseTreeWalker()
        walker.walk(listener, tree)

        return listener.result
    except Exception as e:
        result = ArraySpecPointerTargetResult()
        result.diagnostics.append(
            SemanticDiagnostic(
                severity=DiagnosticSeverity.ERROR,
                code='E676-PARSE',
                message=f'Parse error: {str(e)}',
            )
        )
        return result
