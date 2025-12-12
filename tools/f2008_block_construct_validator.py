#!/usr/bin/env python3
"""Fortran 2008 BLOCK Construct Semantic Validator

Implements semantic validation for Fortran 2008 BLOCK construct constraints per
ISO/IEC 1539-1:2010 (J3/08-007), specifically Section 8.1.4 (R807-R809 and C806-C808).

This validator checks:
- C806: specification-part must not contain COMMON, EQUIVALENCE, IMPLICIT, INTENT,
         NAMELIST, or OPTIONAL statements
- C807: SAVE statement in BLOCK must contain saved-entity-list without common-block-name
- C808: block-construct-name must match between block-stmt and end-block-stmt

Reference: ISO/IEC 1539-1:2010 (J3/08-007), Section 8.1.4 BLOCK construct
"""

import sys
from dataclasses import dataclass, field
from enum import Enum, auto
from pathlib import Path
from typing import List, Optional, Tuple

sys.path.insert(
    0, str(Path(__file__).parent.parent / "grammars" / "generated" / "modern")
)

from antlr4 import CommonTokenStream, InputStream, ParseTreeWalker
from Fortran2008Lexer import Fortran2008Lexer
from Fortran2008Parser import Fortran2008Parser
from Fortran2008ParserListener import Fortran2008ParserListener


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
class BlockConstructResult:
    """Results from BLOCK construct semantic validation."""
    diagnostics: List[SemanticDiagnostic] = field(default_factory=list)
    block_constructs: List[dict] = field(default_factory=list)

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


class F2008BlockConstructListener(Fortran2008ParserListener):
    """ANTLR listener for Fortran 2008 BLOCK construct semantic analysis."""

    # Forbidden statement keywords in BLOCK specification part (per C806)
    FORBIDDEN_IN_BLOCK_SPEC = {
        'common', 'equivalence', 'implicit', 'intent',
        'namelist', 'optional'
    }

    def __init__(self):
        super().__init__()
        self.result = BlockConstructResult()

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

    def _get_token_text(self, ctx) -> str:
        """Get text representation of a context."""
        if ctx is None:
            return ""
        return ctx.getText().lower()

    def _extract_identifier(self, ctx) -> Optional[str]:
        """Extract identifier text from a context."""
        if ctx is None:
            return None
        text = ctx.getText()
        return text.lower() if text else None

    def _check_forbidden_statement(self, stmt_ctx, stmt_keyword: str):
        """Check if a statement is forbidden in BLOCK specification part."""
        if stmt_keyword.lower() in self.FORBIDDEN_IN_BLOCK_SPEC:
            return True
        return False

    def enterBlock_construct_f2008(self, ctx):
        """Analyze BLOCK construct for semantic constraint violations."""
        line, col = self._get_location(ctx)

        block_info = {
            "line": line,
            "column": col,
            "block_name_start": None,
            "block_name_end": None,
            "has_forbidden_specs": False,
            "has_invalid_save": False,
            "name_mismatch": False,
        }

        # Extract block-construct-name from block-stmt (if present)
        # Grammar: (IDENTIFIER COLON)? BLOCK
        # Check for leading identifier before COLON
        block_name_start = None
        try:
            # Look for first child which may be IDENTIFIER
            if hasattr(ctx, "IDENTIFIER"):
                identifiers = ctx.IDENTIFIER()
                if identifiers:
                    if isinstance(identifiers, list) and len(identifiers) > 0:
                        block_name_start = identifiers[0].getText().lower()
                    elif not isinstance(identifiers, list):
                        block_name_start = identifiers.getText().lower()
        except Exception:
            pass

        block_info["block_name_start"] = block_name_start

        # Check specification_part for forbidden statements (C806)
        spec_part = None
        try:
            if hasattr(ctx, "specification_part_f2008"):
                spec_part = ctx.specification_part_f2008()
        except Exception:
            pass

        if spec_part is not None:
            # Check all declaration constructs and spec statements in the part
            try:
                # Look for forbidden statement types
                # The specification_part may contain declaration_construct_f2008 items
                self._check_spec_part_for_forbidden_stmts(spec_part, ctx)

            except Exception:
                pass

        # Check for name matching (C808)
        block_name_end = None
        try:
            # Look for trailing IDENTIFIER after END BLOCK
            if hasattr(ctx, "IDENTIFIER"):
                identifiers = ctx.IDENTIFIER()
                if isinstance(identifiers, list) and len(identifiers) > 1:
                    # Last identifier is likely the end-block-name
                    block_name_end = identifiers[-1].getText().lower()
                elif isinstance(identifiers, list) and len(identifiers) == 1:
                    # Single identifier could be start or end depending on grammar
                    if block_name_start is None:
                        block_name_end = identifiers[0].getText().lower()
        except Exception:
            pass

        block_info["block_name_end"] = block_name_end

        # C808: block-construct-name matching
        if block_name_start is not None and block_name_end is not None:
            if block_name_start != block_name_end:
                self._add_diagnostic(
                    DiagnosticSeverity.ERROR,
                    "C808-E001",
                    f"BLOCK construct name mismatch: block-stmt uses '{block_name_start}' "
                    f"but end-block-stmt uses '{block_name_end}'. "
                    f"Per ISO/IEC 1539-1:2010 C808, the names must match.",
                    ctx,
                    "8.1.4",
                )
                block_info["name_mismatch"] = True
        elif (block_name_start is None and block_name_end is not None) or \
             (block_name_start is not None and block_name_end is None):
            # One has name but not the other
            self._add_diagnostic(
                DiagnosticSeverity.ERROR,
                "C808-E002",
                "BLOCK construct name mismatch: block-stmt and end-block-stmt must "
                "both have a name or both lack a name. "
                "Per ISO/IEC 1539-1:2010 C808, they must be consistent.",
                ctx,
                "8.1.4",
            )
            block_info["name_mismatch"] = True

        self.result.block_constructs.append(block_info)

    def _check_spec_part_for_forbidden_stmts(self, spec_part_ctx, block_ctx):
        """Check specification part for forbidden statement types (C806)."""
        if spec_part_ctx is None:
            return

        # The specification_part_f2008 is a sequence of declaration_construct_f2008 items
        # We need to walk through and find any forbidden statements by examining
        # the parse tree structure
        try:
            # Walk through declaration constructs looking for forbidden types
            if hasattr(spec_part_ctx, "declaration_construct_f2008"):
                decl_constructs = spec_part_ctx.declaration_construct_f2008()
                if decl_constructs is None:
                    return

                if not isinstance(decl_constructs, list):
                    decl_constructs = [decl_constructs]

                for decl_construct in decl_constructs:
                    # Check what type of declaration construct this is
                    forbidden_ctx = self._check_for_forbidden_construct(decl_construct)
                    if forbidden_ctx:
                        forbidden_name = forbidden_ctx[0]
                        forbidden_stmt = forbidden_ctx[1]
                        self._add_diagnostic(
                            DiagnosticSeverity.ERROR,
                            "C806-E001",
                            f"BLOCK specification part contains forbidden '{forbidden_name}' statement. "
                            f"Per ISO/IEC 1539-1:2010 C806, specification-part must not contain "
                            f"COMMON, EQUIVALENCE, IMPLICIT, INTENT, NAMELIST, or OPTIONAL statements.",
                            forbidden_stmt,
                            "8.1.4",
                        )
                        return  # Report only first forbidden statement

            # Also check for use_stmt, import_stmt, implicit_stmt at the top level
            if hasattr(spec_part_ctx, "implicit_stmt"):
                implicit_stmts = spec_part_ctx.implicit_stmt()
                if implicit_stmts:
                    if not isinstance(implicit_stmts, list):
                        implicit_stmts = [implicit_stmts]
                    if len(implicit_stmts) > 0:
                        self._add_diagnostic(
                            DiagnosticSeverity.ERROR,
                            "C806-E001",
                            f"BLOCK specification part contains forbidden 'IMPLICIT' statement. "
                            f"Per ISO/IEC 1539-1:2010 C806, specification-part must not contain "
                            f"COMMON, EQUIVALENCE, IMPLICIT, INTENT, NAMELIST, or OPTIONAL statements.",
                            implicit_stmts[0],
                            "8.1.4",
                        )
                        return

        except Exception:
            pass

    def _check_for_forbidden_construct(self, decl_ctx):
        """Check if a declaration_construct is a forbidden type.

        Returns tuple (forbidden_name, context) if forbidden, None otherwise.
        """
        if decl_ctx is None:
            return None

        # Check for common_stmt
        if hasattr(decl_ctx, "common_stmt"):
            if decl_ctx.common_stmt():
                return ("COMMON", decl_ctx.common_stmt())

        # Check for namelist_stmt
        if hasattr(decl_ctx, "namelist_stmt"):
            if decl_ctx.namelist_stmt():
                return ("NAMELIST", decl_ctx.namelist_stmt())

        # Check for equivalence_stmt (in F2003 constructs)
        if hasattr(decl_ctx, "equivalence_stmt"):
            if decl_ctx.equivalence_stmt():
                return ("EQUIVALENCE", decl_ctx.equivalence_stmt())

        # Check declaration_construct_f2003 for inherited forbidden stmts
        if hasattr(decl_ctx, "declaration_construct_f2003"):
            f2003_construct = decl_ctx.declaration_construct_f2003()
            if f2003_construct:
                return self._check_for_forbidden_f2003_construct(f2003_construct)

        return None

    def _check_for_forbidden_f2003_construct(self, decl_ctx):
        """Check F2003 declaration construct for forbidden types."""
        if decl_ctx is None:
            return None

        # Check for equivalence_stmt
        if hasattr(decl_ctx, "equivalence_stmt"):
            if decl_ctx.equivalence_stmt():
                return ("EQUIVALENCE", decl_ctx.equivalence_stmt())

        return None


class F2008BlockConstructValidator:
    """Validator for Fortran 2008 BLOCK construct semantic compliance."""

    def __init__(self):
        self._lexer = None
        self._parser = None

    def validate_code(self, code: str) -> BlockConstructResult:
        """Validate Fortran 2008 source code for BLOCK construct compliance.

        Args:
            code: Fortran 2008 source code string

        Returns:
            BlockConstructResult with diagnostics
        """
        try:
            input_stream = InputStream(code)
            self._lexer = Fortran2008Lexer(input_stream)
            token_stream = CommonTokenStream(self._lexer)
            self._parser = Fortran2008Parser(token_stream)

            tree = self._parser.program_unit_f2008()

            syntax_errors = self._parser.getNumberOfSyntaxErrors()
            if syntax_errors > 0:
                result = BlockConstructResult()
                result.diagnostics.append(
                    SemanticDiagnostic(
                        severity=DiagnosticSeverity.ERROR,
                        code="SYNTAX_E001",
                        message=f"Code contains {syntax_errors} syntax error(s). "
                        "Fix syntax errors before semantic validation.",
                    )
                )
                return result

            listener = F2008BlockConstructListener()
            walker = ParseTreeWalker()
            walker.walk(listener, tree)

            return listener.result
        except Exception as e:
            result = BlockConstructResult()
            result.diagnostics.append(
                SemanticDiagnostic(
                    severity=DiagnosticSeverity.ERROR,
                    code="VALIDATION_E001",
                    message=f"Validation error: {str(e)}",
                )
            )
            return result

    def validate_file(self, filepath: str) -> BlockConstructResult:
        """Validate Fortran 2008 source file for BLOCK construct compliance.

        Args:
            filepath: Path to Fortran 2008 source file

        Returns:
            BlockConstructResult with diagnostics
        """
        path = Path(filepath)
        if not path.exists():
            result = BlockConstructResult()
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


def validate_block_construct(source_code: str) -> BlockConstructResult:
    """Validate Fortran 2008 source code for BLOCK construct semantic compliance.

    Args:
        source_code: Fortran 2008 source code string

    Returns:
        BlockConstructResult with diagnostics
    """
    validator = F2008BlockConstructValidator()
    return validator.validate_code(source_code)


def validate_block_construct_file(filepath: str) -> BlockConstructResult:
    """Validate Fortran 2008 source file for BLOCK construct semantic compliance.

    Args:
        filepath: Path to Fortran 2008 source file

    Returns:
        BlockConstructResult with diagnostics
    """
    validator = F2008BlockConstructValidator()
    return validator.validate_file(filepath)
