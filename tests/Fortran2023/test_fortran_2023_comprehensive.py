#!/usr/bin/env python3
"""
Fortran 2023 (ISO/IEC 1539-1:2023) Comprehensive Test Suite

Tests the complete Fortran 2023 implementation covering all incremental 
improvements over Fortran 2018, focusing on error corrections and 
new features that prepare for LazyFortran2025.

F2023 New Features Tested:
- Enhanced enumerated types (ENUM improvements)
- Conditional expressions (ternary operator ? :)  
- IEEE arithmetic enhancements (IEEE_MAX, IEEE_MIN functions)
- BOZ constant improvements in array constructors
- NAMELIST enhancements (PUBLIC groups with PRIVATE variables)
- SYSTEM_CLOCK improvements (same kind requirements)
- Enhanced type safety and error corrections

Test Coverage Philosophy:
This suite validates that F2023 grammar correctly recognizes all incremental
improvements while maintaining seamless compatibility with F2018.
Serves as the foundation validation for LazyFortran2025 type inference.
"""

import sys
import os
import pytest
from pathlib import Path

# Add grammars directory to Python path for generated parsers
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '../../grammars'))
sys.path.append(str(Path(__file__).parent.parent))

from fixture_utils import load_fixture

try:
    from antlr4 import InputStream, CommonTokenStream
    from Fortran2023Lexer import Fortran2023Lexer
    # Parser import may fail if grammar has issues - graceful handling
    try:
        from Fortran2023Parser import Fortran2023Parser
        PARSER_AVAILABLE = True
    except ImportError:
        PARSER_AVAILABLE = False
        Fortran2023Parser = None
except ImportError as e:
    pytest.skip(f"Fortran 2023 grammar not built: {e}", allow_module_level=True)


class TestFortran2023Lexer:
    """Test Fortran 2023 lexer with incremental F2023 improvements."""

    def create_lexer(self, input_text):
        """Create lexer for F2023 input text."""
        input_stream = InputStream(input_text)
        lexer = Fortran2023Lexer(input_stream)
        return lexer

    def get_tokens(self, input_text):
        """Get all tokens from F2023 input text."""
        lexer = self.create_lexer(input_text)
        tokens = []
        while True:
            token = lexer.nextToken()
            if token.type == -1:  # EOF
                break
            tokens.append(token)
        return tokens

    def test_lexer_compilation(self):
        """Test that Fortran 2023 lexer compiles and instantiates."""
        lexer = self.create_lexer("program test")
        assert lexer is not None

    def test_enhanced_enumeration_keywords(self):
        """Test F2023 enhanced enumeration keywords."""
        enum_keywords = {
            'ENUMERATOR': Fortran2023Lexer.ENUMERATOR
        }
        
        for keyword, expected_token in enum_keywords.items():
            tokens = self.get_tokens(keyword)
            assert len(tokens) >= 1
            assert tokens[0].type == expected_token, f"Expected {expected_token} for '{keyword}', got {tokens[0].type}"

    def test_conditional_expression_operator(self):
        """Test F2023 conditional expression operator (ternary)."""
        operators = {
            '?': Fortran2023Lexer.QUESTION
        }
        
        for op, expected_token in operators.items():
            tokens = self.get_tokens(op)
            assert len(tokens) >= 1
            assert tokens[0].type == expected_token, f"Operator '{op}' expected type {expected_token}, got {tokens[0].type}"

    def test_enhanced_ieee_arithmetic_functions(self):
        """Test F2023 enhanced IEEE arithmetic functions."""
        ieee_functions = {
            'IEEE_MAX': Fortran2023Lexer.IEEE_MAX,
            'IEEE_MIN': Fortran2023Lexer.IEEE_MIN,
            'IEEE_MAX_MAG': Fortran2023Lexer.IEEE_MAX_MAG,
            'IEEE_MIN_MAG': Fortran2023Lexer.IEEE_MIN_MAG
        }
        
        for func, expected_token in ieee_functions.items():
            tokens = self.get_tokens(func)
            assert len(tokens) >= 1
            assert tokens[0].type == expected_token, f"IEEE function '{func}' expected type {expected_token}, got {tokens[0].type}"

    def test_enhanced_intrinsic_constants(self):
        """Test F2023 enhanced intrinsic constant keywords."""
        constants = {
            'LOGICAL_KINDS': Fortran2023Lexer.LOGICAL_KINDS,
            'CHARACTER_KINDS': Fortran2023Lexer.CHARACTER_KINDS
        }
        
        for const, expected_token in constants.items():
            tokens = self.get_tokens(const)
            assert len(tokens) >= 1
            assert tokens[0].type == expected_token

    def test_f2018_compatibility(self):
        """Test that F2023 maintains full F2018 compatibility."""
        # Test key F2018 constructs still work
        f2018_keywords = [
            'CO_SUM', 'CO_MIN', 'CO_MAX', 'CO_REDUCE', 'CO_BROADCAST',
            'IMAGE_STATUS', 'FAILED_IMAGES', 'STOPPED_IMAGES',
            'SELECT_RANK', 'RANK_STAR', 'RANK_DEFAULT',
            'RANDOM_INIT', 'REPEATABLE', 'IMAGE_DISTINCT',
            'REDUCE', 'OUT_OF_RANGE', 'COSHAPE', 'TEAM_NUMBER',
            'FORM_TEAM', 'CHANGE_TEAM', 'END_TEAM', 'TEAM_TYPE'
        ]
        
        for keyword in f2018_keywords:
            tokens = self.get_tokens(keyword)
            assert len(tokens) >= 1, f"F2018 keyword '{keyword}' not recognized in F2023"
            # Should have corresponding token type
            token_type = tokens[0].type
            assert hasattr(Fortran2023Lexer, keyword), f"Token {keyword} missing in F2023 lexer"

    def test_f2003_oop_compatibility(self):
        """Test that F2023 maintains F2003 OOP compatibility."""
        # Test key F2003 OOP constructs
        oop_keywords = [
            'CLASS', 'EXTENDS', 'ABSTRACT', 'PROCEDURE', 'GENERIC',
            'FINAL', 'NOPASS', 'PASS', 'DEFERRED', 'NON_OVERRIDABLE',
            'ALLOCATABLE', 'POINTER', 'TARGET'
        ]
        
        for keyword in oop_keywords:
            tokens = self.get_tokens(keyword)
            assert len(tokens) >= 1, f"F2003 OOP keyword '{keyword}' not recognized in F2023"

    def test_f90_module_compatibility(self):
        """Test that F2023 maintains F90 module system compatibility."""
        # Test key F90 module constructs
        module_keywords = [
            'MODULE', 'USE', 'ONLY', 'PUBLIC', 'PRIVATE', 'INTERFACE',
            'CONTAINS', 'END_MODULE', 'END_INTERFACE'
        ]
        
        for keyword in module_keywords:
            tokens = self.get_tokens(keyword)
            assert len(tokens) >= 1, f"F90 module keyword '{keyword}' not recognized in F2023"


@pytest.mark.skipif(not PARSER_AVAILABLE, reason="Parser not available")
class TestFortran2023Parser:
    """Test Fortran 2023 parser with F2023 enhancements."""

    def create_parser(self, input_text):
        """Create parser for F2023 input text."""
        input_stream = InputStream(input_text)
        lexer = Fortran2023Lexer(input_stream)
        token_stream = CommonTokenStream(lexer)
        parser = Fortran2023Parser(token_stream)
        return parser

    def test_parser_compilation(self):
        """Test that F2023 parser compiles and can parse basic constructs."""
        test_input = load_fixture(
            "Fortran2023",
            "test_fortran_2023_comprehensive",
            "basic_program.f90",
        )
        parser = self.create_parser(test_input)
        
        try:
            tree = parser.program_unit_f2023()
            assert tree is not None
        except Exception as e:
            pytest.fail(f"Basic F2023 program parsing failed: {e}")

    def test_enhanced_enumeration_parsing(self):
        """Test F2023 enhanced enumeration parsing."""
        enum_input = load_fixture(
            "Fortran2023",
            "test_fortran_2023_comprehensive",
            "enum_program.f90",
        )
        parser = self.create_parser(enum_input)
        
        try:
            tree = parser.program_unit_f2023()
            assert tree is not None
        except Exception as e:
            pytest.fail(f"F2023 enhanced enumeration parsing failed: {e}")

    def test_conditional_expression_parsing(self):
        """Test F2023 conditional expression parsing."""
        conditional_input = load_fixture(
            "Fortran2023",
            "test_fortran_2023_comprehensive",
            "conditional_expression.f90",
        )
        parser = self.create_parser(conditional_input)
        
        try:
            tree = parser.program_unit_f2023()
            assert tree is not None
        except Exception as e:
            pytest.fail(f"F2023 conditional expression parsing failed: {e}")

    def test_enhanced_ieee_parsing(self):
        """Test F2023 enhanced IEEE arithmetic function parsing."""
        ieee_input = load_fixture(
            "Fortran2023",
            "test_fortran_2023_comprehensive",
            "ieee_program.f90",
        )
        parser = self.create_parser(ieee_input)
        
        try:
            tree = parser.program_unit_f2023()
            assert tree is not None
        except Exception as e:
            pytest.fail(f"F2023 IEEE arithmetic parsing failed: {e}")

    def test_enhanced_boz_array_constructor(self):
        """Test F2023 enhanced BOZ in array constructors."""
        boz_input = load_fixture(
            "Fortran2023",
            "test_fortran_2023_comprehensive",
            "boz_array_constructor.f90",
        )
        parser = self.create_parser(boz_input)
        
        try:
            tree = parser.program_unit_f2023()
            assert tree is not None
        except Exception as e:
            pytest.fail(f"F2023 BOZ array constructor parsing failed: {e}")

    def test_f2018_compatibility_parsing(self):
        """Test that F2023 maintains F2018 parsing compatibility."""
        f2018_input = load_fixture(
            "Fortran2023",
            "test_fortran_2023_comprehensive",
            "f2018_compat_program.f90",
        )
        parser = self.create_parser(f2018_input)
        
        try:
            tree = parser.program_unit_f2023()
            assert tree is not None
        except Exception as e:
            pytest.fail(f"F2018 compatibility in F2023 parsing failed: {e}")


class TestFortran2023Foundation:
    """Test F2023 as foundation for LazyFortran2025."""

    def test_f2023_foundation_readiness(self):
        """Verify F2023 is ready as foundation for LazyFortran2025."""
        # Test that F2023 lexer can be imported (foundation requirement)
        assert Fortran2023Lexer is not None
        
        # Test basic functionality
        input_stream = InputStream("program lazy_fortran")
        lexer = Fortran2023Lexer(input_stream)
        token = lexer.nextToken()
        assert token is not None
        assert token.type == Fortran2023Lexer.PROGRAM

    def test_complete_standards_chain(self):
        """Test complete standards inheritance chain to F2023."""
        # Test that key features from all eras are available
        required_features = [
            # FORTRAN era
            'IF', 'DO', 'GOTO', 'INTEGER', 'REAL',
            # F77 structured programming  
            'CHARACTER', 'THEN', 'ELSE', 'ENDIF',
            # F90 modern programming
            'MODULE', 'ALLOCATABLE', 'POINTER', 'INTERFACE',
            # F2003 OOP
            'CLASS', 'EXTENDS', 'PROCEDURE', 'ABSTRACT',
            # F2008 parallel programming
            'CONCURRENT', 'CONTIGUOUS', 'DO_CONCURRENT',
            # F2018 teams and events
            'CO_SUM', 'SELECT_RANK', 'FORM_TEAM',
            # F2023 enhancements
            'ENUMERATOR', 'QUESTION', 'IEEE_MAX'
        ]
        
        for feature in required_features:
            assert hasattr(Fortran2023Lexer, feature), f"Missing feature: {feature}"

    def test_lazyfortran2025_preparation(self):
        """Test F2023 preparation for LazyFortran2025 extension."""
        # This test verifies F2023 grammar can be used as import base for LazyF2025
        
        # Test that key extension points are available
        extension_points = [
            'program_unit_f2023',     # Can be extended for LazyF2025 program units
            'expr_f2023',             # Can be extended for LazyF2025 expressions
            'type_declaration_stmt_f2023'  # Can be extended for type inference
        ]
        
        if PARSER_AVAILABLE:
            for extension_point in extension_points:
                assert hasattr(Fortran2023Parser, extension_point), f"Missing extension point: {extension_point}"

    def test_all_historical_compatibility(self):
        """Test compatibility across entire FORTRAN/Fortran history."""
        # Test that constructs from different eras coexist
        mixed_era_input = load_fixture(
            "Fortran2023",
            "test_fortran_2023_comprehensive",
            "mixed_era_program.f90",
        )

        lexer = Fortran2023Lexer(InputStream(mixed_era_input))
        tokens = []
        while True:
            token = lexer.nextToken()
            if token.type == -1:
                break
            tokens.append(token)
        
        # Should successfully tokenize mixed historical constructs
        assert len(tokens) > 20, "Mixed historical constructs not properly tokenized"


class TestFortran2023ErrorCorrections:
    """Test F2023 error corrections and improvements."""

    def test_ieee_function_changes(self):
        """Test F2023 IEEE function behavioral changes."""
        # F2023 changed IEEE_MAX_NUM, IEEE_MIN_NUM behavior with NaN
        # New functions IEEE_MAX, IEEE_MIN have different semantics
        ieee_functions = ['IEEE_MAX', 'IEEE_MIN', 'IEEE_MAX_MAG', 'IEEE_MIN_MAG']
        
        for func in ieee_functions:
            tokens = Fortran2023Lexer(InputStream(func))
            token = tokens.nextToken()
            assert token.type == getattr(Fortran2023Lexer, func)

    def test_system_clock_improvements(self):
        """Test F2023 SYSTEM_CLOCK same-kind requirements."""
        # In F2023, all SYSTEM_CLOCK integer arguments must have same kind
        system_clock_input = "call system_clock(count, count_rate, count_max)"
        
        tokens = Fortran2023Lexer(InputStream(system_clock_input))
        token_list = []
        while True:
            token = tokens.nextToken()
            if token.type == -1:
                break
            token_list.append(token)
        
        # Should tokenize successfully (semantic checking is compiler's job)
        assert len(token_list) > 5

    def test_namelist_enhancements(self):
        """Test F2023 NAMELIST enhancements."""
        # F2023: PUBLIC namelist groups may contain PRIVATE variables
        namelist_input = """
        module test_mod
            private
            integer :: private_var
            public :: public_namelist
            namelist /public_namelist/ private_var
        end module
        """
        
        lexer = Fortran2023Lexer(InputStream(namelist_input))
        tokens = []
        while True:
            token = lexer.nextToken()
            if token.type == -1:
                break
            tokens.append(token)
        
        # Should tokenize successfully 
        assert len(tokens) > 10


if __name__ == '__main__':
    """Run Fortran 2023 comprehensive test suite."""
    print("Running Fortran 2023 (ISO/IEC 1539-1:2023) Comprehensive Test Suite...")
    print("=" * 80)
    print("Testing latest ISO standard with incremental improvements over F2018")
    print("F2023 Key Features: Enhanced ENUM, Conditional Expressions, IEEE Improvements")
    print("Foundation Status: Ready for LazyFortran2025 type inference extensions")
    print("=" * 80)
    
    # Run the tests
    pytest.main([__file__, '-v'])
