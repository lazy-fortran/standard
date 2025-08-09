#!/usr/bin/env python3
"""
Free-Form Source Format Tests

Tests for the free-form source format implementation that serves as 
the foundation for all modern Fortran standards (F90-F2023).

Test Coverage:
- Free-form tokenization (enhanced identifiers, comments, continuation)
- Enhanced literals (kind specifiers, dual quotes)
- Flexible parsing (no column restrictions)
- Integration with SharedCore inheritance
- Cross-validation preparation for F90+ standards
"""

import sys
import os
import pytest

# Add grammars directory to Python path for generated parsers
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '../../grammars/free_form'))

try:
    from antlr4 import InputStream, CommonTokenStream
    from FreeFormSourceLexer import FreeFormSourceLexer
    # Parser import may fail due to incomplete implementation - that's OK
    try:
        from FreeFormSourceParser import FreeFormSourceParser
        PARSER_AVAILABLE = True
    except ImportError:
        PARSER_AVAILABLE = False
        FreeFormSourceParser = None
except ImportError as e:
    pytest.skip(f"Free-form grammar not built: {e}", allow_module_level=True)


class TestFreeFormLexer:
    """Test free-form source format lexer functionality."""

    def create_lexer(self, input_text):
        """Create lexer for free-form input text."""
        input_stream = InputStream(input_text)
        lexer = FreeFormSourceLexer(input_stream)
        return lexer

    def get_tokens(self, input_text):
        """Get all tokens from input text."""
        lexer = self.create_lexer(input_text)
        tokens = []
        while True:
            token = lexer.nextToken()
            if token.type == -1:  # EOF
                break
            tokens.append(token)
        return tokens

    def test_lexer_compilation(self):
        """Test that free-form lexer compiles and instantiates."""
        lexer = self.create_lexer("program test")
        assert lexer is not None

    def test_enhanced_identifiers(self):
        """Test free-form enhanced identifiers (long, case-insensitive, underscores)."""
        # Long identifiers (F90+: up to 31+ characters)
        long_identifier = "very_long_identifier_name_that_exceeds_six_characters"
        tokens = self.get_tokens(long_identifier)
        
        # Should recognize as single IDENTIFIER token
        assert len(tokens) >= 1
        assert tokens[0].type == FreeFormSourceLexer.IDENTIFIER
        assert tokens[0].text == long_identifier

    def test_case_insensitive_identifiers(self):
        """Test case-insensitive identifier recognition."""
        test_cases = ["Program", "PROGRAM", "program", "ProGrAm"]
        
        for case_variant in test_cases:
            tokens = self.get_tokens(case_variant)
            assert len(tokens) >= 1
            # Should be recognized as IDENTIFIER (case preserved but insensitive)
            assert tokens[0].type == FreeFormSourceLexer.IDENTIFIER

    def test_underscores_in_identifiers(self):
        """Test underscores in identifiers (F90+ enhancement)."""
        identifier_with_underscores = "my_variable_name"
        tokens = self.get_tokens(identifier_with_underscores)
        
        assert len(tokens) >= 1
        assert tokens[0].type == FreeFormSourceLexer.IDENTIFIER
        assert tokens[0].text == identifier_with_underscores

    def test_free_form_comments(self):
        """Test free-form comments (! anywhere on line)."""
        test_input = """
        program test  ! This is a comment
        integer :: x  ! Another comment
        ! Full line comment
        x = 42 ! End of line comment
        """
        
        lexer = self.create_lexer(test_input)
        # Comments should be skipped, so we should only see non-comment tokens
        tokens = self.get_tokens(test_input)
        
        # Should have program, test, integer, ::, x, x, =, 42 (comments skipped)
        assert len(tokens) >= 8
        
        # Verify no comment tokens in output (they're skipped)
        for token in tokens:
            assert token.type != FreeFormSourceLexer.FREE_FORM_COMMENT

    def test_enhanced_numeric_literals(self):
        """Test numeric literals with kind specifiers (F90+ enhancement)."""
        # Integer literals with kind specifiers
        int_literals = ["123", "123_int32", "456_long"]
        
        for literal in int_literals:
            tokens = self.get_tokens(literal)
            assert len(tokens) >= 1
            # Should recognize as INTEGER_LITERAL or INTEGER_LITERAL_KIND
            assert tokens[0].type in [
                FreeFormSourceLexer.INTEGER_LITERAL, 
                FreeFormSourceLexer.INTEGER_LITERAL_KIND
            ]

        # Real literals with kind specifiers  
        real_literals = ["3.14", "3.14_real64", "2.5e-3_dp", "1.0_quad"]
        
        for literal in real_literals:
            tokens = self.get_tokens(literal)
            assert len(tokens) >= 1
            # Should recognize as REAL_LITERAL or REAL_LITERAL_KIND
            assert tokens[0].type in [
                FreeFormSourceLexer.REAL_LITERAL,
                FreeFormSourceLexer.REAL_LITERAL_KIND
            ]

    def test_dual_quote_strings(self):
        """Test dual quote string support (F90+ enhancement)."""
        # Double-quoted strings
        double_quoted = '"Hello, World!"'
        tokens = self.get_tokens(double_quoted)
        assert len(tokens) >= 1
        assert tokens[0].type == FreeFormSourceLexer.DOUBLE_QUOTE_STRING

        # Single-quoted strings
        single_quoted = "'Hello, World!'"
        tokens = self.get_tokens(single_quoted)
        assert len(tokens) >= 1
        assert tokens[0].type == FreeFormSourceLexer.SINGLE_QUOTE_STRING

        # Embedded quotes
        embedded_double = '"He said ""Hello"""'
        tokens = self.get_tokens(embedded_double)
        assert len(tokens) >= 1
        assert tokens[0].type == FreeFormSourceLexer.DOUBLE_QUOTE_STRING

    def test_f90_operators(self):
        """Test F90+ specific operators."""
        operators = ["::", "=>", "%"]
        expected_tokens = [
            FreeFormSourceLexer.DOUBLE_COLON,
            FreeFormSourceLexer.ARROW, 
            FreeFormSourceLexer.PERCENT
        ]
        
        for op, expected in zip(operators, expected_tokens):
            tokens = self.get_tokens(op)
            assert len(tokens) >= 1
            assert tokens[0].type == expected

    def test_array_constructor_brackets(self):
        """Test array constructor brackets (F90+ feature)."""
        test_input = "[1, 2, 3]"
        tokens = self.get_tokens(test_input)
        
        # Should have: [ 1 , 2 , 3 ]
        assert len(tokens) >= 7
        assert tokens[0].type == FreeFormSourceLexer.LBRACKET
        assert tokens[-1].type == FreeFormSourceLexer.RBRACKET

    def test_f90_keywords(self):
        """Test F90+ keywords recognition."""
        keywords = {
            "THEN": FreeFormSourceLexer.THEN,
            "PARAMETER": FreeFormSourceLexer.PARAMETER,
            "DIMENSION": FreeFormSourceLexer.DIMENSION,
            "ALLOCATABLE": FreeFormSourceLexer.ALLOCATABLE,
            "POINTER": FreeFormSourceLexer.POINTER,
            "TARGET": FreeFormSourceLexer.TARGET
        }
        
        for keyword, expected_token in keywords.items():
            tokens = self.get_tokens(keyword)
            assert len(tokens) >= 1
            if tokens[0].type != expected_token:
                print(f"DEBUG: Keyword '{keyword}' got token type {tokens[0].type}, expected {expected_token}")
                print(f"DEBUG: Token text: '{tokens[0].text}'")
            assert tokens[0].type == expected_token

    def test_shared_core_integration(self):
        """Test integration with SharedCore inheritance."""
        # Test that shared core tokens work
        shared_core_input = "A = B + C * D"
        tokens = self.get_tokens(shared_core_input)
        
        # Should recognize inherited operators
        assert len(tokens) >= 7
        
        # Find specific token types
        token_types = [token.type for token in tokens]
        assert FreeFormSourceLexer.IDENTIFIER in token_types  # A, B, C, D
        assert FreeFormSourceLexer.ASSIGN in token_types      # =
        assert FreeFormSourceLexer.PLUS in token_types        # +
        assert FreeFormSourceLexer.MULTIPLY in token_types    # *


@pytest.mark.skipif(not PARSER_AVAILABLE, reason="Parser not available")
class TestFreeFormParser:
    """Test free-form source format parser functionality."""

    def create_parser(self, input_text):
        """Create parser for free-form input text."""
        input_stream = InputStream(input_text)
        lexer = FreeFormSourceLexer(input_stream)
        token_stream = CommonTokenStream(lexer)
        parser = FreeFormSourceParser(token_stream)
        return parser

    def test_parser_compilation(self):
        """Test that free-form parser compiles and can parse basic constructs."""
        test_input = """
        program test
            integer :: x
            x = 42
        end program
        """
        
        parser = self.create_parser(test_input)
        
        # Should not raise exceptions for basic parsing
        try:
            tree = parser.program_unit_free_form()
            assert tree is not None
        except Exception as e:
            pytest.fail(f"Basic free-form parsing failed: {e}")

    def test_flexible_layout(self):
        """Test flexible source layout (no column restrictions)."""
        flexible_input = """
program flexible_test
integer::x,y,z
x=1;y=2
  z=x+y
        end program
        """
        
        parser = self.create_parser(flexible_input)
        
        try:
            tree = parser.program_unit_free_form()
            assert tree is not None
        except Exception as e:
            pytest.fail(f"Flexible layout parsing failed: {e}")

    def test_enhanced_expressions(self):
        """Test enhanced expressions with F90+ features."""
        enhanced_expr = """
        program expr_test
            real(real64) :: pi = 3.14159_real64
            integer :: numbers(3) = [1, 2, 3]
            character(len=*) :: msg = "Hello, World!"
        end program
        """
        
        parser = self.create_parser(enhanced_expr)
        
        try:
            tree = parser.program_unit_free_form()
            assert tree is not None
        except Exception as e:
            pytest.fail(f"Enhanced expression parsing failed: {e}")


class TestFreeFormFoundation:
    """Test free-form as foundation for F90+ standards."""

    def test_foundation_readiness(self):
        """Verify free-form is ready as foundation for F90+ standards."""
        # Test that lexer can be imported (foundation requirement)
        assert FreeFormSourceLexer is not None
        
        # Test basic functionality
        input_stream = InputStream("program test")
        lexer = FreeFormSourceLexer(input_stream)
        token = lexer.nextToken()
        assert token is not None

    def test_extension_preparation(self):
        """Test preparation for F90+ standard extensions."""
        # This test verifies the grammar can be used as an import base
        
        # Test that key F90+ tokens are available
        required_f90_tokens = [
            'DOUBLE_COLON', 'ARROW', 'PERCENT', 'LBRACKET', 'RBRACKET',
            'ALLOCATABLE', 'POINTER', 'TARGET', 'THEN'
        ]
        
        for token_name in required_f90_tokens:
            assert hasattr(FreeFormSourceLexer, token_name), f"Missing F90+ token: {token_name}"

    def test_shared_core_compatibility(self):
        """Test compatibility with SharedCore inheritance."""
        # Test that SharedCore tokens are inherited
        required_shared_tokens = [
            'ASSIGN', 'PLUS', 'MINUS', 'MULTIPLY', 'DIVIDE', 'POWER',
            'INTEGER_LITERAL', 'REAL_LITERAL', 'IDENTIFIER'
        ]
        
        for token_name in required_shared_tokens:
            assert hasattr(FreeFormSourceLexer, token_name), f"Missing SharedCore token: {token_name}"


if __name__ == '__main__':
    """Run free-form source format tests."""
    print("Running Free-Form Source Format Tests...")
    print("=" * 60)
    print("Testing foundation for modern Fortran standards (F90-F2023)")
    print("Implementation Status: Foundation for F90+ standards")
    print("=" * 60)
    
    # Run the tests
    pytest.main([__file__, '-v'])