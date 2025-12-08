#!/usr/bin/env python3
"""TDD tests for Issue #23 - F2003 Procedure Pointers"""

import sys
import pytest
from pathlib import Path

sys.path.insert(0, 'grammars/generated/modern')
sys.path.append(str(Path(__file__).parent.parent))

from antlr4 import *
from Fortran2003Lexer import Fortran2003Lexer
from Fortran2003Parser import Fortran2003Parser
from fixture_utils import load_fixture

class TestIssue23TDD:
    """Test-driven development for Issue #23 procedure pointer fixes"""
    
    def parse_code(self, code):
        """Parse Fortran 2003 code and return (tree, errors)"""
        input_stream = InputStream(code)
        lexer = Fortran2003Lexer(input_stream)
        parser = Fortran2003Parser(CommonTokenStream(lexer))
        
        tree = parser.program_unit_f2003()
        errors = parser.getNumberOfSyntaxErrors()
        return tree, errors
    
    def test_abstract_interface_declaration(self):
        """RED: Abstract interface should parse"""
        code = load_fixture(
            "Fortran2003",
            "test_issue23_tdd",
            "abstract_interface_module.f90",
        )
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"Abstract interface should parse, got {errors}"
    
    def test_basic_procedure_pointer_declaration(self):
        """RED: Basic procedure pointer declaration should parse"""
        code = load_fixture(
            "Fortran2003",
            "test_issue23_tdd",
            "basic_procedure_pointer_declaration.f90",
        )
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"Procedure pointer declaration should parse, got {errors}"
    
    def test_procedure_pointer_component(self):
        """RED: Procedure pointer as type component should parse"""
        code = load_fixture(
            "Fortran2003",
            "test_issue23_tdd",
            "procedure_pointer_component.f90",
        )
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"Procedure pointer component should parse, got {errors}"
    
    def test_procedure_pointer_assignment(self):
        """RED: Procedure pointer assignment should parse"""
        code = load_fixture(
            "Fortran2003",
            "test_issue23_tdd",
            "procedure_pointer_assignment_program.f90",
        )
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"Procedure pointer assignment should parse, got {errors}"
    
    def test_procedure_call_via_pointer(self):
        """RED: Procedure call through pointer should parse"""
        code = load_fixture(
            "Fortran2003",
            "test_issue23_tdd",
            "procedure_call_via_pointer_program.f90",
        )
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"Procedure call via pointer should parse, got {errors}"
    
    def test_multiple_procedure_pointers(self):
        """RED: Multiple procedure pointer declarations should parse"""
        code = load_fixture(
            "Fortran2003",
            "test_issue23_tdd",
            "multiple_procedure_pointers_module.f90",
        )
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"Multiple procedure pointers should parse, got {errors}"
    
    def test_procedure_pointer_in_type_with_assignment(self):
        """RED: Type with procedure pointer and assignment should parse"""
        code = load_fixture(
            "Fortran2003",
            "test_issue23_tdd",
            "procedure_pointer_in_type_program.f90",
        )
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"Type with procedure pointer assignment should parse, got {errors}"

if __name__ == "__main__":
    pytest.main([__file__, "-v"])
