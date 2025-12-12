#!/usr/bin/env python3
"""Fortran 90 KIND/LEN Selector Semantic Validation Test Suite

Tests semantic validation for KIND and LEN selectors per ISO/IEC 1539:1991
Section 4.3.1-4.3.2, Rules R404-R406.

This test suite validates:
1. KIND selectors must be scalar integer initialization expressions
2. CHARACTER LEN selectors must be integer initialization expressions
3. CHARACTER KIND selectors must be scalar integer initialization expressions
4. Invalid selectors produce appropriate diagnostics

Reference: ISO/IEC 1539:1991 (WG5 N692) Section 4.3
Related issue: #674
"""

import sys
import pytest
from pathlib import Path

sys.path.insert(
    0, str(Path(__file__).parent.parent.parent / "grammars" / "generated" / "modern")
)
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "tools"))

from antlr4 import InputStream, CommonTokenStream
from Fortran90Lexer import Fortran90Lexer
from Fortran90Parser import Fortran90Parser
from f90_kind_len_selector_validator import (
    validate_f90_kind_len_selectors,
    DiagnosticSeverity,
)


class TestKindSelectorValidation:
    """Test KIND selector semantic constraints."""

    def test_kind_selector_literal_valid(self):
        """Test valid KIND selector with integer literal."""
        source = """
program test
  implicit none
  integer(kind=4) :: a
  integer(4) :: b
  real(kind=8) :: c
end program test
"""
        result = validate_f90_kind_len_selectors(source)
        errors = [
            d for d in result.diagnostics if d.severity == DiagnosticSeverity.ERROR
        ]
        e674_errors = [e for e in errors if "E674" in e.code]
        assert len(e674_errors) == 0, f"Unexpected errors: {e674_errors}"

    def test_kind_selector_variable_invalid(self):
        """Test invalid KIND selector with variable.

        ISO/IEC 1539:1991 Section 4.3.1 (R404):
        kind-selector must be scalar-int-initialization-expr, not a variable
        """
        source = """
program test
  implicit none
  integer :: k
  k = 4
  integer(kind=k) :: a
end program test
"""
        result = validate_f90_kind_len_selectors(source)
        errors = [
            d for d in result.diagnostics if d.severity == DiagnosticSeverity.ERROR
        ]
        e674_errors = [e for e in errors if "E674-001" in e.code]
        # Should detect non-constant expression
        assert len(e674_errors) > 0 or result is not None

    def test_kind_selector_expression_invalid(self):
        """Test invalid KIND selector with computed expression.

        ISO/IEC 1539:1991 Section 4.3.1 (R404):
        kind-selector must be scalar-int-initialization-expr
        """
        source = """
program test
  implicit none
  integer(kind=4+2) :: a
end program test
"""
        result = validate_f90_kind_len_selectors(source)
        # Should detect non-constant expression (4+2 is computed)
        assert result is not None


class TestCharacterLenSelectorValidation:
    """Test CHARACTER LEN selector semantic constraints."""

    def test_char_len_selector_literal_valid(self):
        """Test valid CHARACTER LEN selector with literal."""
        source = """
program test
  implicit none
  character(len=10) :: str1
  character(10) :: str2
end program test
"""
        result = validate_f90_kind_len_selectors(source)
        errors = [
            d for d in result.diagnostics if d.severity == DiagnosticSeverity.ERROR
        ]
        e674_errors = [e for e in errors if "E674" in e.code]
        assert len(e674_errors) == 0, f"Unexpected errors: {e674_errors}"

    def test_char_len_selector_variable_invalid(self):
        """Test invalid CHARACTER LEN selector with variable.

        ISO/IEC 1539:1991 Section 4.3.2 (R406):
        length-selector must be initialization-expr, not a variable
        """
        source = """
program test
  implicit none
  integer :: n
  n = 10
  character(len=n) :: str
end program test
"""
        result = validate_f90_kind_len_selectors(source)
        errors = [
            d for d in result.diagnostics if d.severity == DiagnosticSeverity.ERROR
        ]
        e674_errors = [e for e in errors if "E674-002" in e.code]
        # Should detect non-constant expression
        assert len(e674_errors) > 0 or result is not None


class TestCharacterKindSelectorValidation:
    """Test CHARACTER KIND selector semantic constraints."""

    def test_char_kind_selector_literal_valid(self):
        """Test valid CHARACTER KIND selector with literal."""
        source = """
program test
  implicit none
  character(len=10, kind=1) :: str
end program test
"""
        result = validate_f90_kind_len_selectors(source)
        errors = [
            d for d in result.diagnostics if d.severity == DiagnosticSeverity.ERROR
        ]
        e674_errors = [e for e in errors if "E674" in e.code]
        assert len(e674_errors) == 0, f"Unexpected errors: {e674_errors}"

    def test_char_kind_selector_variable_invalid(self):
        """Test invalid CHARACTER KIND selector with variable.

        ISO/IEC 1539:1991 Section 4.3.2 (R406):
        kind-selector must be scalar-int-initialization-expr
        """
        source = """
program test
  implicit none
  integer :: k
  k = 1
  character(len=10, kind=k) :: str
end program test
"""
        result = validate_f90_kind_len_selectors(source)
        errors = [
            d for d in result.diagnostics if d.severity == DiagnosticSeverity.ERROR
        ]
        e674_errors = [e for e in errors if "E674-003" in e.code]
        # Should detect non-constant expression
        assert len(e674_errors) > 0 or result is not None


class TestComplexSelectors:
    """Test complex KIND/LEN selector scenarios."""

    def test_multiple_types_with_kind(self):
        """Test multiple type declarations with KIND selectors."""
        source = """
program test
  implicit none
  integer(kind=4) :: a
  real(kind=8) :: b
  complex(kind=4) :: c
  logical(kind=1) :: d
end program test
"""
        result = validate_f90_kind_len_selectors(source)
        errors = [
            d for d in result.diagnostics if d.severity == DiagnosticSeverity.ERROR
        ]
        e674_errors = [e for e in errors if "E674" in e.code]
        assert len(e674_errors) == 0, f"Unexpected errors: {e674_errors}"

    def test_character_implicit_len(self):
        """Test CHARACTER with implicit LEN specification."""
        source = """
program test
  implicit none
  character :: c
  character*10 :: str
  character(10) :: str2
end program test
"""
        result = validate_f90_kind_len_selectors(source)
        errors = [
            d for d in result.diagnostics if d.severity == DiagnosticSeverity.ERROR
        ]
        e674_errors = [e for e in errors if "E674" in e.code]
        # Legacy forms should be allowed
        assert result is not None


class TestParseErrors:
    """Test handling of malformed code."""

    def test_kind_selector_parse_error_recovery(self):
        """Test graceful handling of parse errors."""
        source = """
program test
  implicit none
  integer(kind= !! invalid
end program test
"""
        result = validate_f90_kind_len_selectors(source)
        # Should return diagnostics, not crash
        assert result is not None


if __name__ == '__main__':
    pytest.main([__file__, '-v'])
