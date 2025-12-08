#!/usr/bin/env python3
"""IEEE arithmetic tests for Issue #27

Test comprehensive F2003 IEEE floating-point support including:
- IEEE intrinsic module usage
- Exception handling
- Special value detection  
- Rounding mode control
- IEEE features specification
"""

import sys
import pytest
from pathlib import Path

sys.path.insert(0, 'grammars/generated/modern')
sys.path.append(str(Path(__file__).parent.parent))

from antlr4 import *
from Fortran2003Lexer import Fortran2003Lexer
from Fortran2003Parser import Fortran2003Parser
from fixture_utils import load_fixture

class TestIssue27IEEEArithmetic:
    """Comprehensive IEEE arithmetic functionality tests"""
    
    def parse_code(self, code):
        """Parse Fortran 2003 code and return (tree, errors)"""
        input_stream = InputStream(code)
        lexer = Fortran2003Lexer(input_stream)
        parser = Fortran2003Parser(CommonTokenStream(lexer))
        
        tree = parser.program_unit_f2003()
        errors = parser.getNumberOfSyntaxErrors()
        return tree, errors
    
    def test_ieee_exceptions_module_import(self):
        """IEEE exceptions module import should parse"""
        code = load_fixture(
            "Fortran2003",
            "test_issue27_ieee_arithmetic",
            "ieee_exceptions_module.f90",
        )
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"IEEE exceptions module import failed: {errors} errors"
    
    def test_ieee_arithmetic_module_import(self):
        """IEEE arithmetic module import should parse"""
        code = load_fixture(
            "Fortran2003",
            "test_issue27_ieee_arithmetic",
            "ieee_arithmetic_module.f90",
        )
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"IEEE arithmetic module import failed: {errors} errors"
    
    def test_ieee_features_module_import(self):
        """IEEE features module import should parse"""
        code = load_fixture(
            "Fortran2003",
            "test_issue27_ieee_arithmetic",
            "ieee_features_module.f90",
        )
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"IEEE features module import failed: {errors} errors"
    
    def test_ieee_exceptions_only_import(self):
        """IEEE exceptions with ONLY clause should parse"""
        code = load_fixture(
            "Fortran2003",
            "test_issue27_ieee_arithmetic",
            "ieee_exceptions_only_module.f90",
        )
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"IEEE exceptions ONLY import failed: {errors} errors"
    
    def test_ieee_arithmetic_only_import(self):
        """IEEE arithmetic with ONLY clause should parse"""
        code = load_fixture(
            "Fortran2003",
            "test_issue27_ieee_arithmetic",
            "ieee_arithmetic_only_module.f90",
        )
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"IEEE arithmetic ONLY import failed: {errors} errors"
    
    def test_ieee_logical_operators_fix(self):
        """Test that logical operators work in IEEE expressions (specific fix validation)"""
        code = load_fixture(
            "Fortran2003",
            "test_issue27_ieee_arithmetic",
            "ieee_logical_operators_program.f90",
        )
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"IEEE logical operators test failed: {errors} errors"
    
    
    def test_ieee_exception_handling_basic(self):
        """Basic IEEE exception handling should parse"""
        code = load_fixture(
            "Fortran2003",
            "test_issue27_ieee_arithmetic",
            "ieee_exception_handling_program.f90",
        )
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"IEEE exception handling failed: {errors} errors"
    
    def test_ieee_special_values(self):
        """IEEE special value detection should parse"""
        code = load_fixture(
            "Fortran2003",
            "test_issue27_ieee_arithmetic",
            "ieee_special_values_program.f90",
        )
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"IEEE special values failed: {errors} errors"
    
    def test_ieee_rounding_modes(self):
        """IEEE rounding mode control should parse"""
        code = load_fixture(
            "Fortran2003",
            "test_issue27_ieee_arithmetic",
            "ieee_rounding_modes_program.f90",
        )
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"IEEE rounding modes failed: {errors} errors"
    
    def test_ieee_comprehensive_example(self):
        """Comprehensive IEEE usage pattern should parse"""
        code = load_fixture(
            "Fortran2003",
            "test_issue27_ieee_arithmetic",
            "ieee_comprehensive_module.f90",
        )
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"Comprehensive IEEE example failed: {errors} errors"

if __name__ == "__main__":
    pytest.main([__file__, "-v"])
