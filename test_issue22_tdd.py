#!/usr/bin/env python3
"""TDD tests for Issue #22 - Advanced F2003 OOP features"""

import sys
import pytest
sys.path.insert(0, 'grammars')

from antlr4 import *
from Fortran2003Lexer import Fortran2003Lexer
from Fortran2003Parser import Fortran2003Parser

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
        code = """module test_mod
    type :: shape_t
    contains
        procedure :: area_method
    end type shape_t
end module test_mod"""
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"Basic type-bound procedure should parse, got {errors}"
    
    def test_type_bound_procedure_with_binding(self):
        """RED: Type-bound procedure with => binding should parse"""
        code = """module test_mod
    type :: shape_t
    contains
        procedure :: area => area_impl
    end type shape_t
end module test_mod"""
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"Type-bound procedure with binding should parse, got {errors}"
    
    def test_deferred_procedure_in_abstract_type(self):
        """RED: DEFERRED procedure in abstract type should parse"""
        code = """module test_mod
    type, abstract :: shape_t
    contains
        procedure(area_interface), deferred :: area_method
    end type shape_t
end module test_mod"""
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"DEFERRED procedure should parse, got {errors}"
    
    def test_generic_type_bound_procedure(self):
        """RED: GENERIC type-bound procedure should parse"""
        code = """module test_mod
    type :: vector_t
    contains
        generic :: operator(+) => add_vectors
    end type vector_t
end module test_mod"""
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"GENERIC type-bound procedure should parse, got {errors}"
    
    def test_final_procedure(self):
        """RED: FINAL procedure should parse"""
        code = """module test_mod
    type :: resource_t
    contains
        final :: destroy_resource
    end type resource_t
end module test_mod"""
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"FINAL procedure should parse, got {errors}"
    
    def test_pass_nopass_attributes(self):
        """RED: PASS and NOPASS attributes should parse"""
        code = """module test_mod
    type :: wrapper_t
    contains
        procedure, pass :: method_with_pass
        procedure, nopass :: static_method
    end type wrapper_t
end module test_mod"""
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"PASS/NOPASS attributes should parse, got {errors}"
    
    def test_multiple_procedure_bindings(self):
        """RED: Multiple procedure bindings in one statement should parse"""
        code = """module test_mod
    type :: math_t
    contains
        procedure :: add_int, add_real
        generic :: operator(+) => add_int, add_real
    end type math_t
end module test_mod"""
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"Multiple procedure bindings should parse, got {errors}"

if __name__ == "__main__":
    pytest.main([__file__, "-v"])