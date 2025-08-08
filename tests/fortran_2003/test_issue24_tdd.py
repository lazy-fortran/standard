#!/usr/bin/env python3
"""TDD tests for Issue #24 - F2003 C Type Token Support

These tests validate that C interoperability type tokens are recognized
and can be used as standalone type declarations. Full BIND(C) syntax
and ISO_C_BINDING module support are planned for future iterations.
"""

import sys
import pytest
sys.path.insert(0, 'grammars')

from antlr4 import *
from Fortran2003Lexer import Fortran2003Lexer
from Fortran2003Parser import Fortran2003Parser

class TestIssue24TDD:
    """Test-driven development for Issue #24 C type token support"""
    
    def parse_code(self, code):
        """Parse Fortran 2003 code and return (tree, errors)"""
        input_stream = InputStream(code)
        lexer = Fortran2003Lexer(input_stream)
        parser = Fortran2003Parser(CommonTokenStream(lexer))
        
        tree = parser.program_unit_f2003()
        errors = parser.getNumberOfSyntaxErrors()
        return tree, errors
    
    def test_c_float_type_token(self):
        """C_FLOAT type token should be recognized"""
        code = """module test
    c_float :: x
end module test"""
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"C_FLOAT type token should parse, got {errors}"
    
    def test_multiple_c_type_tokens(self):
        """Multiple C type tokens should be recognized"""
        code = """module test
    c_int :: i
    c_float :: x
end module test"""
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"Multiple C type tokens should parse, got {errors}"
    
    def test_value_attribute_in_procedure(self):
        """VALUE attribute should parse"""
        code = """module procedures
integer, value :: x
end module procedures"""
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"VALUE attribute in procedures should parse, got {errors}"
    
    def test_c_pointer_declarations(self):
        """C pointer type declarations should parse"""
        code = """module pointers
c_ptr :: cptr
c_funptr :: fptr
end module pointers"""
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"C pointer declarations should parse, got {errors}"
    
    def test_c_double_type_token(self):
        """C_DOUBLE type token should be recognized"""
        code = """module bindings
c_double :: x
c_double :: res
end module bindings"""
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"C_DOUBLE type token should parse, got {errors}"
    
    def test_c_interop_type_variety(self):
        """Various C interop types should be recognized"""
        code = """module isobinding
c_int :: i
c_float :: x
c_ptr :: ptr
c_null_ptr :: nullp
end module isobinding"""
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"Various C interop types should parse, got {errors}"
    
    def test_comprehensive_c_interop_example(self):
        """Comprehensive C type test should parse"""
        code = """module test
    c_int8_t :: i8
    c_int16_t :: i16  
    c_int32_t :: i32
    c_int64_t :: i64
    c_float :: f
    c_double :: d
    c_long_double :: ld
    c_bool :: b
    c_char :: ch
end module test"""
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"Comprehensive C interop should parse, got {errors}"

if __name__ == "__main__":
    pytest.main([__file__, "-v"])