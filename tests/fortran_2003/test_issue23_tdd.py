#!/usr/bin/env python3
"""TDD tests for Issue #23 - F2003 Procedure Pointers"""

import sys
import pytest
sys.path.insert(0, 'grammars')

from antlr4 import *
from Fortran2003Lexer import Fortran2003Lexer
from Fortran2003Parser import Fortran2003Parser

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
        code = """module test_mod
    abstract interface
        function func_interface(x) result(y)
            real, intent(in) :: x
            real :: y
        end function
    end interface
end module test_mod"""
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"Abstract interface should parse, got {errors}"
    
    def test_basic_procedure_pointer_declaration(self):
        """RED: Basic procedure pointer declaration should parse"""
        code = """module test_mod
    abstract interface
        function func_interface(x) result(y)
            real, intent(in) :: x
            real :: y
        end function
    end interface
    
    procedure(func_interface), pointer :: func_ptr
end module test_mod"""
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"Procedure pointer declaration should parse, got {errors}"
    
    def test_procedure_pointer_component(self):
        """RED: Procedure pointer as type component should parse"""
        code = """module test_mod
    abstract interface
        function func_interface(x) result(y)
            real, intent(in) :: x
            real :: y
        end function
    end interface
    
    type :: math_t
        procedure(func_interface), pointer, nopass :: operation
    end type math_t
end module test_mod"""
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"Procedure pointer component should parse, got {errors}"
    
    def test_procedure_pointer_assignment(self):
        """RED: Procedure pointer assignment should parse"""
        code = """program test_prog
    abstract interface
        function func_interface(x) result(y)
            real, intent(in) :: x
            real :: y
        end function
    end interface
    
    procedure(func_interface), pointer :: func_ptr
    
    func_ptr => actual_function
end program test_prog"""
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"Procedure pointer assignment should parse, got {errors}"
    
    def test_procedure_call_via_pointer(self):
        """RED: Procedure call through pointer should parse"""
        code = """program test_prog
    abstract interface
        function func_interface(x) result(y)
            real, intent(in) :: x
            real :: y
        end function
    end interface
    
    procedure(func_interface), pointer :: func_ptr
    real :: result, value
    
    result = func_ptr(value)
end program test_prog"""
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"Procedure call via pointer should parse, got {errors}"
    
    def test_multiple_procedure_pointers(self):
        """RED: Multiple procedure pointer declarations should parse"""
        code = """module test_mod
    abstract interface
        function func_interface(x) result(y)
            real, intent(in) :: x
            real :: y
        end function
        subroutine sub_interface(x)
            real, intent(inout) :: x
        end subroutine
    end interface
    
    procedure(func_interface), pointer :: func_ptr1, func_ptr2
    procedure(sub_interface), pointer :: sub_ptr
end module test_mod"""
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"Multiple procedure pointers should parse, got {errors}"
    
    def test_procedure_pointer_in_type_with_assignment(self):
        """RED: Type with procedure pointer and assignment should parse"""
        code = """program test_prog
    abstract interface
        subroutine operation_interface(x, y)
            real, intent(in) :: x
            real, intent(out) :: y
        end subroutine
    end interface
    
    type :: processor_t
        procedure(operation_interface), pointer, nopass :: operation
    end type processor_t
    
    type(processor_t) :: obj
    
    obj%operation => some_procedure
end program test_prog"""
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"Type with procedure pointer assignment should parse, got {errors}"

if __name__ == "__main__":
    pytest.main([__file__, "-v"])