#!/usr/bin/env python3
"""
LFortran Grammar Test Suite

Tests the LFortran grammar (F2023 + J3 Generics):

1. J3 Generics (Fortran 202Y preview):
   - TEMPLATE construct with deferred type parameters
   - REQUIREMENT construct for type constraints
   - REQUIRE statement for constraint specifications
   - INSTANTIATE statement for explicit instantiation

2. Fortran 2023 compatibility (inherited)

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
    """Parse source code and return parse tree with zero syntax errors."""
    if not PARSER_AVAILABLE:
        pytest.skip("LFortran parser not generated")
    input_stream = InputStream(source)
    lexer = LFortranLexer(input_stream)
    token_stream = CommonTokenStream(lexer)
    parser = LFortranParser(token_stream)
    tree = parser.program_lfortran()
    errors = parser.getNumberOfSyntaxErrors()
    if errors > 0:
        raise SyntaxError(f"LFortran parser reported {errors} syntax error(s)")
    return tree


WALRUS_FIXTURES = [
    "infer_walrus_01.f90",
    "infer_walrus_02.f90",
    "infer_walrus_03.f90",
    "infer_walrus_array_01.f90",
    "infer_walrus_array_02.f90",
    "infer_walrus_array_03.f90",
    "infer_walrus_shadow_01.f90",
    "infer_walrus_shadow_02.f90",
    "infer_walrus_shadow_03.f90",
    "infer_walrus_struct_01.f90",
    "infer_walrus_struct_02.f90",
    "infer_walrus_struct_03.f90",
]

WALRUS_SEMANTIC_SHAPE_FIXTURES = [
    "infer_walrus_error_01.f90",
    "infer_walrus_error_02.f90",
    "infer_walrus_redecl_01.f90",
]


# =============================================================================
# J3 GENERICS: TEMPLATE CONSTRUCT TESTS
# =============================================================================

class TestTemplateConstruct:
    """Test TEMPLATE construct parsing."""

    def test_template_keyword_recognized(self):
        """Verify TEMPLATE keyword is tokenized correctly."""
        tokens = tokenize("template swap_t{T}")
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
template empty_t{T}
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
        tokens = tokenize("requirement comparable{T, less_than}")
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
requirement addable{T}
    type, deferred :: T
end requirement addable
"""
        try:
            tree = parse(source)
            assert tree is not None
        except Exception as e:
            pytest.fail(f"Minimal requirement parsing failed: {e}")

    def test_requirement_abstract_interface_fixture(self):
        """Test requirement with abstract interface body (not interface block)."""
        source = load_fixture("requirement_abstract_interface.f90")
        try:
            tree = parse(source)
            assert tree is not None
        except Exception as e:
            pytest.fail(f"Requirement with abstract interface parsing failed: {e}")


# =============================================================================
# J3 GENERICS: REQUIRES STATEMENT TESTS
# =============================================================================

class TestRequiresStatement:
    """Test REQUIRES statement parsing (J3/24-107r1)."""

    def test_requires_keyword_recognized(self):
        """Verify REQUIRES keyword is tokenized correctly."""
        tokens = tokenize("requires :: comparable{T, less_than}")
        token_types = [t.type for t in tokens]
        assert LFortranLexer.REQUIRES_KW in token_types

    def test_template_with_requires_fixture(self):
        """Test template using REQUIRES constraint."""
        source = load_fixture("template_with_require.f90")
        try:
            tree = parse(source)
            assert tree is not None
        except Exception as e:
            pytest.fail(f"Template with REQUIRES parsing failed: {e}")


# =============================================================================
# J3 GENERICS: INSTANTIATE STATEMENT TESTS
# =============================================================================

class TestInstantiateStatement:
    """Test INSTANTIATE statement parsing."""

    def test_instantiate_keyword_recognized(self):
        """Verify INSTANTIATE keyword is tokenized correctly."""
        tokens = tokenize("instantiate swap_t{integer}")
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
program test_instantiate
    instantiate swap_t{real}, only: swap_dp => swap
end program test_instantiate
"""
        try:
            tree = parse(source)
            assert tree is not None
        except Exception as e:
            pytest.fail(f"Instantiate with kind parsing failed: {e}")

    def test_instantiate_no_only_fixture(self):
        """Test instantiation without ONLY clause."""
        source = load_fixture("instantiate_no_only.f90")
        try:
            tree = parse(source)
            assert tree is not None
        except Exception as e:
            pytest.fail(f"Instantiate without ONLY parsing failed: {e}")

    def test_instantiate_multiple_types_fixture(self):
        """Test instantiation with multiple type parameters."""
        source = load_fixture("instantiate_multiple_types.f90")
        try:
            tree = parse(source)
            assert tree is not None
        except Exception as e:
            pytest.fail(f"Instantiate with multiple types parsing failed: {e}")

    def test_instantiate_derived_type_fixture(self):
        """Test instantiation with derived type parameter."""
        source = load_fixture("instantiate_derived_type.f90")
        try:
            tree = parse(source)
            assert tree is not None
        except Exception as e:
            pytest.fail(f"Instantiate with derived type parsing failed: {e}")


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
# J3 GENERICS: SIMPLE TEMPLATE PROCEDURES
# =============================================================================

class TestSimpleTemplateProcedures:
    """Test simple template procedure syntax (J3 24-107r1).

    Simple template procedures use curly braces in the procedure name:
        function mysum{T}(x, y) result(z)
        subroutine swap{T}(x, y)
    """

    def test_simple_template_function_fixture(self):
        """Test simple template function with {T} syntax."""
        source = load_fixture("simple_template_function.f90")
        try:
            tree = parse(source)
            assert tree is not None
        except Exception as e:
            pytest.fail(f"Simple template function parsing failed: {e}")

    def test_simple_template_subroutine_fixture(self):
        """Test simple template subroutine with {T} syntax."""
        source = load_fixture("simple_template_subroutine.f90")
        try:
            tree = parse(source)
            assert tree is not None
        except Exception as e:
            pytest.fail(f"Simple template subroutine parsing failed: {e}")

    def test_simple_template_multiple_params_fixture(self):
        """Test simple template with multiple type parameters {T, U}."""
        source = load_fixture("simple_template_multiple_params.f90")
        try:
            tree = parse(source)
            assert tree is not None
        except Exception as e:
            pytest.fail(f"Simple template with multiple params parsing failed: {e}")

    def test_simple_template_function_inline(self):
        """Test inline simple template function."""
        source = """
function add{T}(x, y) result(z)
    type, deferred :: T
    type(T), intent(in) :: x, y
    type(T) :: z
    z = x + y
end function add
"""
        try:
            tree = parse(source)
            assert tree is not None
        except Exception as e:
            pytest.fail(f"Inline simple template function parsing failed: {e}")


# =============================================================================
# J3 GENERICS: INLINE INSTANTIATION (CURLY BRACES)
# =============================================================================

class TestInlineInstantiationCurly:
    """Test inline instantiation with curly braces (J3 24-107r1).

    Syntax: template-name{type-list}(args)
    Example: call swap{integer}(a, b)
    """

    def test_inline_curly_fixture(self):
        """Test inline instantiation with curly braces."""
        source = load_fixture("inline_instantiate_curly.f90")
        try:
            tree = parse(source)
            assert tree is not None
        except Exception as e:
            pytest.fail(f"Inline instantiation (curly) parsing failed: {e}")

    def test_inline_curly_subroutine_call(self):
        """Test inline instantiation in subroutine call."""
        source = """
program test
    integer :: a, b
    a = 1
    b = 2
    call swap{integer}(a, b)
end program test
"""
        try:
            tree = parse(source)
            assert tree is not None
        except Exception as e:
            pytest.fail(f"Inline curly subroutine call parsing failed: {e}")

    def test_inline_curly_function_call(self):
        """Test inline instantiation in function call."""
        source = """
program test
    integer :: a, b, c
    a = 1
    b = 2
    c = mysum{integer}(a, b)
end program test
"""
        try:
            tree = parse(source)
            assert tree is not None
        except Exception as e:
            pytest.fail(f"Inline curly function call parsing failed: {e}")

    def test_inline_curly_with_kind(self):
        """Test inline instantiation with kind specifier."""
        source = """
program test
    real(8) :: x, y
    x = 1.0d0
    y = 2.0d0
    call swap{real(8)}(x, y)
end program test
"""
        try:
            tree = parse(source)
            assert tree is not None
        except Exception as e:
            pytest.fail(f"Inline curly with kind parsing failed: {e}")


# =============================================================================
# J3 GENERICS: INLINE INSTANTIATION (CARET)
# =============================================================================

class TestInlineInstantiationCaret:
    """Test inline instantiation with caret syntax (J3 r4 revision).

    Syntax: template-name^(type-list)(args)
    Example: call swap^(integer)(a, b)
    """

    def test_inline_caret_fixture(self):
        """Test inline instantiation with caret syntax."""
        source = load_fixture("inline_instantiate_caret.f90")
        try:
            tree = parse(source)
            assert tree is not None
        except Exception as e:
            pytest.fail(f"Inline instantiation (caret) parsing failed: {e}")

    def test_inline_caret_subroutine_call(self):
        """Test inline instantiation with caret in subroutine call."""
        source = """
program test
    integer :: a, b
    a = 1
    b = 2
    call swap^(integer)(a, b)
end program test
"""
        try:
            tree = parse(source)
            assert tree is not None
        except Exception as e:
            pytest.fail(f"Inline caret subroutine call parsing failed: {e}")

    def test_inline_caret_function_call(self):
        """Test inline instantiation with caret in function call."""
        source = """
program test
    integer :: a, b, c
    a = 1
    b = 2
    c = mysum^(integer)(a, b)
end program test
"""
        try:
            tree = parse(source)
            assert tree is not None
        except Exception as e:
            pytest.fail(f"Inline caret function call parsing failed: {e}")

    def test_inline_caret_with_kind(self):
        """Test inline instantiation with caret and kind specifier."""
        source = """
program test
    real(8) :: x, y
    x = 1.0d0
    y = 2.0d0
    call swap^(real(8))(x, y)
end program test
"""
        try:
            tree = parse(source)
            assert tree is not None
        except Exception as e:
            pytest.fail(f"Inline caret with kind parsing failed: {e}")

    def test_inline_with_kind_fixture(self):
        """Test inline instantiation with kind specifiers (both syntaxes)."""
        source = load_fixture("inline_instantiate_with_kind.f90")
        try:
            tree = parse(source)
            assert tree is not None
        except Exception as e:
            pytest.fail(f"Inline with kind parsing failed: {e}")


# =============================================================================
# TYPE INFERENCE / WALRUS TESTS
# =============================================================================

class TestInferWalrus:
    """Test walrus syntax and infer-related parsing in LFortran grammar."""

    def test_walrus_token_recognized(self):
        """Verify := is tokenized as COLON_EQUAL."""
        tokens = tokenize("x := 1")
        token_types = [t.type for t in tokens]
        assert LFortranLexer.COLON_EQUAL in token_types

    @pytest.mark.parametrize("fixture", WALRUS_FIXTURES)
    def test_walrus_fixture_parses(self, fixture):
        """Walrus feature fixtures should parse without syntax errors."""
        source = load_fixture(fixture)
        try:
            tree = parse(source)
            assert tree is not None
        except Exception as e:
            pytest.fail(f"Walrus fixture parsing failed ({fixture}): {e}")

    @pytest.mark.parametrize("fixture", WALRUS_SEMANTIC_SHAPE_FIXTURES)
    def test_walrus_semantic_error_shapes_still_parse(self, fixture):
        """Semantic-error shapes from upstream should stay syntax-valid here."""
        source = load_fixture(fixture)
        try:
            tree = parse(source)
            assert tree is not None
        except Exception as e:
            pytest.fail(f"Walrus semantic-shape parsing failed ({fixture}): {e}")


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

    template swap_t{T}
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


if __name__ == '__main__':
    print("Running LFortran Grammar Test Suite...")
    print("=" * 80)
    print("Testing J3 Generics: TEMPLATE, REQUIREMENT, REQUIRES, INSTANTIATE")
    print("=" * 80)
    pytest.main([__file__, '-v'])
