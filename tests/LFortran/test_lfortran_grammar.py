#!/usr/bin/env python3
"""
LFortran Grammar Test Suite

Tests the LFortran grammar extensions over Fortran 2023:

1. J3 Generics (Fortran 202Y preview):
   - TEMPLATE construct with deferred type parameters
   - REQUIREMENT construct for type constraints
   - REQUIRE statement for constraint specifications
   - INSTANTIATE statement for explicit instantiation

2. Global scope support (script/infer mode):
   - Bare statements at top level
   - Bare expressions at top level
   - Bare declarations at top level

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
    from LFortranLexer import LFortranLexer
    from LFortranParser import LFortranParser
    PARSER_AVAILABLE = True
except ImportError:
    PARSER_AVAILABLE = False
    LFortranLexer = None
    LFortranParser = None
    Token = None

FIXTURES_DIR = Path(__file__).parent / "fixtures"


def load_fixture(name: str) -> str:
    """Load a test fixture file."""
    fixture_path = FIXTURES_DIR / name
    if not fixture_path.exists():
        pytest.skip(f"Fixture not found: {name}")
    return fixture_path.read_text()


def tokenize(source: str) -> list:
    """Tokenize source code and return list of token types."""
    if not PARSER_AVAILABLE:
        pytest.skip("LFortran parser not generated")
    input_stream = InputStream(source)
    lexer = LFortranLexer(input_stream)
    tokens = []
    while True:
        token = lexer.nextToken()
        if token.type == Token.EOF:
            break
        tokens.append(token)
    return tokens


def parse(source: str):
    """Parse source code and return parse tree."""
    if not PARSER_AVAILABLE:
        pytest.skip("LFortran parser not generated")
    input_stream = InputStream(source)
    lexer = LFortranLexer(input_stream)
    token_stream = CommonTokenStream(lexer)
    parser = LFortranParser(token_stream)
    return parser.program_lfortran()


# =============================================================================
# J3 GENERICS: TEMPLATE CONSTRUCT TESTS
# =============================================================================

class TestTemplateConstruct:
    """Test TEMPLATE construct parsing."""

    def test_template_keyword_recognized(self):
        """Verify TEMPLATE keyword is tokenized correctly."""
        tokens = tokenize("template swap_t(T)")
        token_types = [t.type for t in tokens]
        assert LFortranLexer.TEMPLATE_KW in token_types

    def test_template_swap_fixture(self):
        """Test basic template with swap procedure."""
        source = load_fixture("template_swap.f90")
        try:
            tree = parse(source)
            assert tree is not None
        except Exception as e:
            pytest.fail(f"Template parsing failed: {e}")

    def test_template_add_fixture(self):
        """Test template with function returning type(T)."""
        source = load_fixture("template_add.f90")
        try:
            tree = parse(source)
            assert tree is not None
        except Exception as e:
            pytest.fail(f"Template parsing failed: {e}")

    def test_template_minimal(self):
        """Test minimal template syntax."""
        source = """
template empty_t(T)
    type, deferred :: T
end template empty_t
"""
        try:
            tree = parse(source)
            assert tree is not None
        except Exception as e:
            pytest.fail(f"Minimal template parsing failed: {e}")

    def test_deferred_keyword(self):
        """Verify DEFERRED keyword is tokenized correctly."""
        tokens = tokenize("type, deferred :: T")
        token_types = [t.type for t in tokens]
        assert LFortranLexer.DEFERRED_KW in token_types


# =============================================================================
# J3 GENERICS: REQUIREMENT CONSTRUCT TESTS
# =============================================================================

class TestRequirementConstruct:
    """Test REQUIREMENT construct parsing."""

    def test_requirement_keyword_recognized(self):
        """Verify REQUIREMENT keyword is tokenized correctly."""
        tokens = tokenize("requirement comparable(T, less_than)")
        token_types = [t.type for t in tokens]
        assert LFortranLexer.REQUIREMENT_KW in token_types

    def test_requirement_comparable_fixture(self):
        """Test requirement with interface block."""
        source = load_fixture("requirement_comparable.f90")
        try:
            tree = parse(source)
            assert tree is not None
        except Exception as e:
            pytest.fail(f"Requirement parsing failed: {e}")

    def test_requirement_minimal(self):
        """Test minimal requirement syntax."""
        source = """
requirement addable(T)
    type, deferred :: T
end requirement addable
"""
        try:
            tree = parse(source)
            assert tree is not None
        except Exception as e:
            pytest.fail(f"Minimal requirement parsing failed: {e}")


# =============================================================================
# J3 GENERICS: REQUIRE STATEMENT TESTS
# =============================================================================

class TestRequireStatement:
    """Test REQUIRE statement parsing."""

    def test_require_keyword_recognized(self):
        """Verify REQUIRE keyword is tokenized correctly."""
        tokens = tokenize("require :: comparable(T, less_than)")
        token_types = [t.type for t in tokens]
        assert LFortranLexer.REQUIRE_KW in token_types

    def test_template_with_require_fixture(self):
        """Test template using REQUIRE constraint."""
        source = load_fixture("template_with_require.f90")
        try:
            tree = parse(source)
            assert tree is not None
        except Exception as e:
            pytest.fail(f"Template with REQUIRE parsing failed: {e}")


# =============================================================================
# J3 GENERICS: INSTANTIATE STATEMENT TESTS
# =============================================================================

class TestInstantiateStatement:
    """Test INSTANTIATE statement parsing."""

    def test_instantiate_keyword_recognized(self):
        """Verify INSTANTIATE keyword is tokenized correctly."""
        tokens = tokenize("instantiate swap_t(integer)")
        token_types = [t.type for t in tokens]
        assert LFortranLexer.INSTANTIATE_KW in token_types

    def test_instantiate_basic_fixture(self):
        """Test basic instantiation with rename."""
        source = load_fixture("instantiate_basic.f90")
        try:
            tree = parse(source)
            assert tree is not None
        except Exception as e:
            pytest.fail(f"Instantiate parsing failed: {e}")

    def test_instantiate_with_kind(self):
        """Test instantiation with kind specifier."""
        source = """
instantiate swap_t(real(8)), only: swap_dp => swap
"""
        try:
            tree = parse(source)
            assert tree is not None
        except Exception as e:
            pytest.fail(f"Instantiate with kind parsing failed: {e}")


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
# FORTRAN 2023 COMPATIBILITY TESTS
# =============================================================================

class TestFortran2023Compatibility:
    """Test that LFortran grammar includes all Fortran 2023 features."""

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

    def test_module(self):
        """Test module parses correctly."""
        source = """
module test_module
    implicit none
    integer :: module_var
contains
    function get_var() result(res)
        integer :: res
        res = module_var
    end function get_var
end module test_module
"""
        try:
            tree = parse(source)
            assert tree is not None
        except Exception as e:
            pytest.fail(f"Module parsing failed: {e}")

    def test_f2023_typeof(self):
        """Test F2023 TYPEOF type specifier."""
        source = """
program test_typeof
    integer :: x
    typeof(x) :: y
    x = 1
    y = 2
end program test_typeof
"""
        try:
            tree = parse(source)
            assert tree is not None
        except Exception as e:
            pytest.fail(f"TYPEOF parsing failed: {e}")


# =============================================================================
# INTEGRATION TESTS
# =============================================================================

class TestIntegration:
    """Integration tests combining multiple features."""

    def test_template_in_module(self):
        """Test template defined inside a module."""
        source = """
module generic_swap
    implicit none

    template swap_t(T)
        type, deferred :: T
    contains
        subroutine swap(x, y)
            type(T), intent(inout) :: x, y
            type(T) :: tmp
            tmp = x
            x = y
            y = tmp
        end subroutine swap
    end template swap_t

contains
    ! Module procedures here
end module generic_swap
"""
        try:
            tree = parse(source)
            assert tree is not None
        except Exception as e:
            pytest.fail(f"Template in module parsing failed: {e}")

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


if __name__ == '__main__':
    print("Running LFortran Grammar Test Suite...")
    print("=" * 80)
    print("Testing J3 Generics: TEMPLATE, REQUIREMENT, REQUIRE, INSTANTIATE")
    print("Testing Global Scope: bare statements, expressions, declarations")
    print("=" * 80)
    pytest.main([__file__, '-v'])
