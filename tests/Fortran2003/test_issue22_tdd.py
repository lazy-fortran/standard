#!/usr/bin/env python3
"""TDD tests for Issue #22 - Advanced F2003 OOP features"""

import sys
import pytest
from pathlib import Path

sys.path.insert(0, 'grammars')
sys.path.append(str(Path(__file__).parent.parent))

from antlr4 import *
from Fortran2003Lexer import Fortran2003Lexer
from Fortran2003Parser import Fortran2003Parser
from fixture_utils import load_fixture

class TestIssue22TDD:
    """Test-driven development for Issue #22 advanced OOP fixes"""
    
    def parse_code(self, code):
        """Parse Fortran 2003 code and return (tree, errors)"""
        input_stream = InputStream(code)
        lexer = Fortran2003Lexer(input_stream)
        parser = Fortran2003Parser(CommonTokenStream(lexer))
        
        tree = parser.program_unit_f2003()
        errors = parser.getNumberOfSyntaxErrors()
        return tree, errors
    
    def test_basic_type_bound_procedure(self):
        """RED: Basic type-bound procedure should parse"""
        code = load_fixture(
            "Fortran2003",
            "test_issue22_tdd",
            "basic_type_bound_procedure.f90",
        )
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"Basic type-bound procedure should parse, got {errors}"
    
    def test_type_bound_procedure_with_binding(self):
        """RED: Type-bound procedure with => binding should parse"""
        code = load_fixture(
            "Fortran2003",
            "test_issue22_tdd",
            "type_bound_with_binding.f90",
        )
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"Type-bound procedure with binding should parse, got {errors}"
    
    def test_deferred_procedure_in_abstract_type(self):
        """RED: DEFERRED procedure in abstract type should parse"""
        code = load_fixture(
            "Fortran2003",
            "test_issue22_tdd",
            "deferred_procedure_abstract_type.f90",
        )
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"DEFERRED procedure should parse, got {errors}"
    
    def test_generic_type_bound_procedure(self):
        """RED: GENERIC type-bound procedure should parse"""
        code = load_fixture(
            "Fortran2003",
            "test_issue22_tdd",
            "generic_type_bound_procedure.f90",
        )
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"GENERIC type-bound procedure should parse, got {errors}"
    
    def test_final_procedure(self):
        """RED: FINAL procedure should parse"""
        code = load_fixture(
            "Fortran2003",
            "test_issue22_tdd",
            "final_procedure.f90",
        )
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"FINAL procedure should parse, got {errors}"
    
    def test_pass_nopass_attributes(self):
        """RED: PASS and NOPASS attributes should parse"""
        code = load_fixture(
            "Fortran2003",
            "test_issue22_tdd",
            "pass_nopass_attributes.f90",
        )
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"PASS/NOPASS attributes should parse, got {errors}"
    
    def test_multiple_procedure_bindings(self):
        """RED: Multiple procedure bindings in one statement should parse"""
        code = load_fixture(
            "Fortran2003",
            "test_issue22_tdd",
            "multiple_procedure_bindings.f90",
        )
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"Multiple procedure bindings should parse, got {errors}"

if __name__ == "__main__":
    pytest.main([__file__, "-v"])
