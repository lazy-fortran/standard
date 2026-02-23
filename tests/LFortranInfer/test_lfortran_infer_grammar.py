#!/usr/bin/env python3
"""
LFortranInfer Grammar Test Suite

Tests the LFortranInfer grammar (LFortran + Global Scope / Script Mode):

Global scope features:
   - Bare statements at top level without program/end program
   - Bare expressions at top level (REPL mode)
   - Bare declarations at top level
   - Bare use/implicit statements at top level

Reference: LFortran compiler (https://lfortran.org)
"""

import sys
import os
import pytest
from pathlib import Path

# Add grammars directory to Python path for generated parsers
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '../../grammars/generated/modern'))

# Try to import generated parser components
try:
    from antlr4 import CommonTokenStream, InputStream, Token
    from LFortranInferLexer import LFortranInferLexer
    from LFortranInferParser import LFortranInferParser
    PARSER_AVAILABLE = True
except ImportError:
    PARSER_AVAILABLE = False
    LFortranInferLexer = None
    LFortranInferParser = None
    Token = None

FIXTURES_DIR = Path(__file__).parent / "fixtures"


def load_fixture(name: str) -> str:
    """Load a test fixture file."""
    fixture_path = FIXTURES_DIR / name
    if not fixture_path.exists():
        pytest.skip(f"Fixture not found: {name}")
    return fixture_path.read_text()


def parse(source: str):
    """Parse source code and return parse tree with zero syntax errors."""
    if not PARSER_AVAILABLE:
        pytest.skip("LFortranInfer parser not generated")
    input_stream = InputStream(source)
    lexer = LFortranInferLexer(input_stream)
    token_stream = CommonTokenStream(lexer)
    parser = LFortranInferParser(token_stream)
    tree = parser.program_lfortran_infer()
    errors = parser.getNumberOfSyntaxErrors()
    if errors > 0:
        raise SyntaxError(f"LFortranInfer parser reported {errors} syntax error(s)")
    return tree


INFER_FIXTURES = [
    "infer_first_assignment_01.f90",
    "infer_first_assignment_derived_01.f90",
    "infer_walrus_global_01.f90",
    "infer_walrus_global_02.f90",
    "infer_walrus_shadow_01.f90",
    "infer_walrus_error_shape_01.f90",
]

TRAITS_INFER_POSITIVE_FIXTURES = [
    "traits_infer_script_valid.f90",
]

TRAITS_INFER_NEGATIVE_FIXTURES = [
    "traits_infer_script_invalid.f90",
]


# =============================================================================
# GLOBAL SCOPE TESTS
# =============================================================================

class TestGlobalScope:
    """Test global scope / script mode parsing."""

    def test_global_scope_basic_fixture(self):
        """Test bare statements without program wrapper."""
        source = load_fixture("global_scope_basic.f90")
        try:
            tree = parse(source)
            assert tree is not None
        except Exception as e:
            pytest.fail(f"Global scope basic parsing failed: {e}")

    def test_global_scope_arrays_fixture(self):
        """Test global scope with arrays and expressions."""
        source = load_fixture("global_scope_arrays.f90")
        try:
            tree = parse(source)
            assert tree is not None
        except Exception as e:
            pytest.fail(f"Global scope arrays parsing failed: {e}")

    def test_global_scope_with_functions_fixture(self):
        """Test global scope with function definitions."""
        source = load_fixture("global_scope_with_functions.f90")
        try:
            tree = parse(source)
            assert tree is not None
        except Exception as e:
            pytest.fail(f"Global scope with functions parsing failed: {e}")

    def test_bare_expression(self):
        """Test bare expression at global scope."""
        source = "2 + 2\n"
        try:
            tree = parse(source)
            assert tree is not None
        except Exception as e:
            pytest.fail(f"Bare expression parsing failed: {e}")

    def test_bare_declaration(self):
        """Test bare declaration at global scope."""
        source = "integer :: x\n"
        try:
            tree = parse(source)
            assert tree is not None
        except Exception as e:
            pytest.fail(f"Bare declaration parsing failed: {e}")

    def test_bare_assignment(self):
        """Test bare assignment at global scope."""
        source = "x = 42\n"
        try:
            tree = parse(source)
            assert tree is not None
        except Exception as e:
            pytest.fail(f"Bare assignment parsing failed: {e}")

    def test_bare_print(self):
        """Test bare print statement at global scope."""
        source = 'print *, "Hello, World!"\n'
        try:
            tree = parse(source)
            assert tree is not None
        except Exception as e:
            pytest.fail(f"Bare print parsing failed: {e}")


# =============================================================================
# MIXED MODE TESTS
# =============================================================================

class TestMixedMode:
    """Test mixing script-style and program units."""

    def test_mixed_script_and_program(self):
        """Test mixing script-style and program units."""
        source = """
! Script-style declarations
use iso_fortran_env, only: dp => real64

! Standard program unit
program main
    implicit none
    real(dp) :: x
    x = 1.0_dp
    print *, x
end program main
"""
        try:
            tree = parse(source)
            assert tree is not None
        except Exception as e:
            pytest.fail(f"Mixed script and program parsing failed: {e}")

    def test_script_with_template(self):
        """Test script mode with J3 generics template."""
        source = """
! Script-style: bare use
use iso_fortran_env, only: dp => real64

! Template definition (J3 generics)
template numeric_t{T}
    type, deferred :: T
contains
    function double(x) result(res)
        type(T), intent(in) :: x
        type(T) :: res
        res = x + x
    end function double
end template numeric_t

! Script-style: bare declaration and statement
real(dp) :: value
value = 3.14_dp
print *, value
"""
        try:
            tree = parse(source)
            assert tree is not None
        except Exception as e:
            pytest.fail(f"Script with template parsing failed: {e}")


# =============================================================================
# INFER MODE / WALRUS TESTS
# =============================================================================

class TestInferModeWalrus:
    """Test infer-mode fixtures for first-assignment and walrus syntax."""

    @pytest.mark.parametrize("fixture", INFER_FIXTURES)
    def test_infer_mode_fixtures_parse(self, fixture):
        """Infer-mode fixtures should parse without syntax errors."""
        source = load_fixture(fixture)
        try:
            tree = parse(source)
            assert tree is not None
        except Exception as e:
            pytest.fail(f"Infer-mode fixture parsing failed ({fixture}): {e}")


# =============================================================================
# TRAITS INFER TESTS
# =============================================================================

class TestInferModeTraits:
    """Test trait syntax in infer/script mode."""

    @pytest.mark.parametrize("fixture", TRAITS_INFER_POSITIVE_FIXTURES)
    def test_traits_infer_fixture_parses(self, fixture):
        """Trait fixtures should parse in infer mode."""
        source = load_fixture(fixture)
        try:
            tree = parse(source)
            assert tree is not None
        except Exception as e:
            pytest.fail(f"Infer trait fixture parsing failed ({fixture}): {e}")

    @pytest.mark.parametrize("fixture", TRAITS_INFER_NEGATIVE_FIXTURES)
    def test_traits_infer_invalid_fixture_fails(self, fixture):
        """Malformed trait syntax should fail in infer mode."""
        source = load_fixture(fixture)
        with pytest.raises(SyntaxError):
            parse(source)


# =============================================================================
# FORTRAN 2028 COMPATIBILITY TESTS (inherited)
# =============================================================================

class TestFortran2028Compatibility:
    """Test that LFortranInfer inherits Fortran 2028 features."""

    def test_standard_program(self):
        """Test standard program unit parses correctly."""
        source = """
program test
    implicit none
    integer :: x
    x = 42
    print *, x
end program test
"""
        try:
            tree = parse(source)
            assert tree is not None
        except Exception as e:
            pytest.fail(f"Standard program parsing failed: {e}")


if __name__ == '__main__':
    print("Running LFortranInfer Grammar Test Suite...")
    print("=" * 80)
    print("Testing Global Scope: bare statements, expressions, declarations")
    print("=" * 80)
    pytest.main([__file__, '-v'])
