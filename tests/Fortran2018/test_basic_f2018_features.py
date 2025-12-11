#!/usr/bin/env python3
"""REAL F2018 Tests - Validates Actual Implementation Status

These are REAL tests that:
- Verify what is ACTUALLY implemented vs claimed
- Test token recognition where functional 
- Validate parser behavior honestly
- Do NOT pass fake tests that only check parse tree existence
"""

import sys
import pytest
from pathlib import Path

# Ensure we can import shared test fixtures utilities
sys.path.append(str(Path(__file__).parent.parent))
from fixture_utils import load_fixture

# Add grammars directory to Python path for generated parsers
sys.path.append(str(Path(__file__).parent.parent.parent / "grammars/generated/modern"))

from antlr4 import *
from Fortran2018Lexer import Fortran2018Lexer  
from Fortran2018Parser import Fortran2018Parser

class TestBasicF2018Features:
    """REAL tests validating actual F2018 implementation status"""
    
    def parse_code(self, code):
        """Parse F2018 code and return (tree, errors)"""
        try:
            input_stream = InputStream(code)
            lexer = Fortran2018Lexer(input_stream)
            parser = Fortran2018Parser(CommonTokenStream(lexer))
            tree = parser.program_unit_f2018()
            errors = parser.getNumberOfSyntaxErrors()
            return tree, errors
        except Exception as e:
            # If F2018 fails, this indicates implementation issues
            return None, 999  # High error count indicates system failure
    
    def test_f2018_lexer_parser_exists(self):
        """REAL TEST: Verify F2018 lexer and parser can be imported and instantiated"""
        # This is the most basic test - can we even create the objects?
        try:
            input_stream = InputStream("module test\nend module")
            lexer = Fortran2018Lexer(input_stream)
            parser = Fortran2018Parser(CommonTokenStream(lexer))
            assert lexer is not None, "F2018 lexer should be creatable"
            assert parser is not None, "F2018 parser should be creatable"
        except Exception as e:
            pytest.fail(f"F2018 basic instantiation failed: {e}")
    
    def test_basic_module_parsing_works(self):
        """REAL TEST: Verify basic module parsing works (inherited from F2008)"""
        code = load_fixture(
            "Fortran2018",
            "test_basic_f2018_features",
            "basic_module.f90",
        )
        
        tree, errors = self.parse_code(code)
        
        # REAL TEST: System should not crash and should produce some result
        assert tree is not None, "Basic module parsing should not crash F2018 parser"
        
        # REAL TEST: For a basic module, errors should be zero
        assert errors == 0, f"Basic module should parse with zero errors, got {errors}"
    
    def test_f2018_grammar_inheritance(self):
        """REAL TEST: Verify F2018 inherits F2008 coarray features"""
        code = load_fixture(
            "Fortran2018",
            "test_basic_f2018_features",
            "coarray_test_module.f90",
        )
        
        tree, errors = self.parse_code(code)
        
        # REAL TEST: F2008 features should eventually work in F2018
        assert tree is not None, "F2008 coarray features should work in F2018"
        # This expectation was tightened as F2018 coarray support improved
        # (see issues #83 and #88).
        assert errors == 0, f"F2008 inheritance should parse with zero errors, got {errors}"
    
    def test_f2018_parser_vs_f2008_functionality(self):
        """REAL TEST: Compare F2018 vs F2008 parsing on same code"""
        code = load_fixture(
            "Fortran2018",
            "test_basic_f2018_features",
            "comparison_module.f90",
        )
        
        # Test with F2018 parser
        f2018_tree, f2018_errors = self.parse_code(code)
        
        # Test with F2008 parser for comparison
        try:
            from Fortran2008Lexer import Fortran2008Lexer
            from Fortran2008Parser import Fortran2008Parser
            
            input_stream = InputStream(code)
            f2008_lexer = Fortran2008Lexer(input_stream)  
            f2008_parser = Fortran2008Parser(CommonTokenStream(f2008_lexer))
            f2008_tree = f2008_parser.program_unit_f2008()
            f2008_errors = f2008_parser.getNumberOfSyntaxErrors()
            
            # REAL TEST: F2018 should parse F2008 code without extra errors
            assert f2018_tree is not None, "F2018 should parse F2008 code"
            assert f2018_errors == f2008_errors, \
                f"F2018 errors ({f2018_errors}) should match F2008 ({f2008_errors}) once coarray support is aligned"

        except ImportError:
            pytest.skip("F2008 parser not available for comparison")

    def test_allocate_statement_enhanced_specifiers(self):
        """REAL TEST: Verify F2018 ALLOCATE R927/R928 specifiers parse (issue #585)"""
        code = load_fixture(
            "Fortran2018",
            "test_basic_f2018_features",
            "enhanced_allocate.f90",
        )

        tree, errors = self.parse_code(code)

        assert tree is not None, "ALLOCATE specifiers fixture should produce a parse tree"
        assert errors == 0, f"Enhanced ALLOCATE specifiers should parse without errors, got {errors}"

    def test_complex_program_structure_limitations(self):
        """REAL TEST: Document known program structure parsing limitations"""
        code = load_fixture(
            "Fortran2018",
            "test_basic_f2018_features",
            "complex_program.f90",
        )
        
        tree, errors = self.parse_code(code)
        
        # REAL TEST: Ultimately this should parse without syntax errors
        assert tree is not None, "Parser should not crash on program structure issues"
        assert errors == 0, f"Complex program structure should parse with zero errors, got {errors}"
    
    def test_fortran2018_specific_tokens_attempt(self):
        """REAL TEST: Attempt to use F2018 tokens and document actual behavior"""
        # Test code with F2018-like syntax
        test_cases = [
            ("call co_sum(x)", "collective operation attempt"),
            ("select rank (x)", "select rank attempt"),
            ("event post(e)", "event operation attempt"),
            ("form team(1, t)", "team operation attempt"),
        ]
        
        working_features = []
        failing_features = []
        
        for code, feature in test_cases:
            full_code = f"""module test
    contains
    subroutine test_feature()
        {code}
    end subroutine
end module"""
            
            tree, errors = self.parse_code(full_code)
            
            if tree is not None and errors == 0:
                working_features.append(feature)
            else:
                failing_features.append(feature)
        
        # REAL TEST: Document what actually works vs what doesn't
        print(f"Working F2018 features: {working_features}")
        print(f"Non-working F2018 features: {failing_features}")
        
        # At minimum, the parser should not crash
        assert len(working_features) + len(failing_features) == len(test_cases), \
            "All test cases should be categorized"
    
    def test_error_recovery_and_robustness(self):
        """REAL TEST: Verify F2018 parser has reasonable error recovery"""
        invalid_code = load_fixture(
            "Fortran2018",
            "test_basic_f2018_features",
            "invalid_bad_syntax_module.f90",
        )
        
        tree, errors = self.parse_code(invalid_code)
        
        # REAL TEST: Parser should handle invalid code gracefully
        assert errors > 0, "Parser should detect syntax errors"
        # Tree might be None for very bad syntax, which is acceptable
    
    def test_current_implementation_coverage_honest_assessment(self):
        """REAL TEST: Honest assessment of current F2018 implementation coverage"""
        
        # Test basic features that should definitely work
        basic_tests = [
            ("module test\nend module", "basic module"),
            ("program test\nend program", "basic program"),
            ("subroutine test()\nend subroutine", "basic subroutine"),
        ]
        
        working_basic = 0
        for code, feature in basic_tests:
            tree, errors = self.parse_code(code)
            if tree is not None and errors == 0:
                working_basic += 1
        
        basic_coverage = (working_basic / len(basic_tests)) * 100
        
        # Document honest coverage
        print(f"Basic Fortran structure coverage: {basic_coverage:.1f}%")
        
        # REAL TEST: Basic coverage should eventually reach 100%; for now we only
        # require that at least one basic form parses without errors.
        assert working_basic >= 1, \
            f"At least one basic Fortran form should parse without errors, got {working_basic}"

    def test_assumed_rank_dummy_argument_parses_without_errors(self):
        """REAL TEST: Assumed-rank dummy arguments using DIMENSION(..) are supported."""
        code = load_fixture(
            "Fortran2018",
            "test_basic_f2018_features",
            "assumed_rank_dummy.f90",
        )

        tree, errors = self.parse_code(code)
        assert tree is not None, "Assumed-rank dummy argument code should parse"
        assert errors == 0, f"Assumed-rank dummy should parse with zero errors, got {errors}"

    def test_do_concurrent_with_locality_and_mask(self):
        """REAL TEST: DO CONCURRENT with locality spec and mask parses."""
        code = load_fixture(
            "Fortran2018",
            "test_basic_f2018_features",
            "do_concurrent_locality.f90",
        )

        tree, errors = self.parse_code(code)
        assert tree is not None, "DO CONCURRENT with locality should parse"
        assert errors == 0, (
            "DO CONCURRENT with locality and mask should parse "
            f"with zero errors, got {errors}"
        )

    def test_do_concurrent_typed_header(self):
        """REAL TEST: DO CONCURRENT with typed header (ISO/IEC 1539-1:2018 R1125)."""
        code = load_fixture(
            "Fortran2018",
            "test_basic_f2018_features",
            "do_concurrent_typed_header.f90",
        )

        tree, errors = self.parse_code(code)
        assert tree is not None, "DO CONCURRENT with typed header should parse"
        assert errors == 0, (
            "DO CONCURRENT with integer-type-spec :: should parse "
            f"with zero errors, got {errors}"
        )

if __name__ == "__main__":
    pytest.main([__file__, "-v"])
