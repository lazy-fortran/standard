#!/usr/bin/env python3
"""TDD tests for Issue #24 - F2003 C Interoperability"""

import sys
import pytest
sys.path.insert(0, 'grammars')

from antlr4 import *
from Fortran2003Lexer import Fortran2003Lexer
from Fortran2003Parser import Fortran2003Parser

class TestIssue24TDD:
    """Test-driven development for Issue #24 C interoperability fixes"""
    
    def parse_code(self, code):
        """Parse Fortran 2003 code and return (tree, errors)"""
        input_stream = InputStream(code)
        lexer = Fortran2003Lexer(input_stream)
        parser = Fortran2003Parser(CommonTokenStream(lexer))
        
        tree = parser.program_unit_f2003()
        errors = parser.getNumberOfSyntaxErrors()
        return tree, errors
    
    def test_bind_c_procedure_interface(self):
        """RED: BIND(C) procedure interface should parse"""
        code = """module c_interface_mod
    interface
        subroutine c_function(x) bind(c, name="c_function")
            use iso_c_binding, only: c_float
            real(c_float), value :: x
        end subroutine
    end interface
end module c_interface_mod"""
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"BIND(C) procedure interface should parse, got {errors}"
    
    def test_bind_c_type_declaration(self):
        """RED: BIND(C) type declaration should parse"""
        code = """module c_types_mod
    use iso_c_binding
    
    type, bind(c) :: c_struct_t
        integer(c_int) :: i
        real(c_float) :: x
    end type c_struct_t
end module c_types_mod"""
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"BIND(C) type declaration should parse, got {errors}"
    
    def test_value_attribute_in_procedure(self):
        """RED: VALUE attribute in procedure arguments should parse"""
        code = """module c_procedures_mod
    use iso_c_binding
    
    interface
        subroutine interface_sub(x, y) bind(c)
            integer(c_int), value :: x
            real(c_float), intent(in) :: y
        end subroutine
    end interface
end module c_procedures_mod"""
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"VALUE attribute in procedures should parse, got {errors}"
    
    def test_c_pointer_declarations(self):
        """RED: C pointer declarations should parse"""
        code = """module c_pointers_mod
    use iso_c_binding
    implicit none
    
    type(c_ptr) :: cptr
    integer(c_int), pointer :: fptr(:)
end module c_pointers_mod"""
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"C pointer declarations should parse, got {errors}"
    
    def test_c_function_with_name_binding(self):
        """RED: C function with name binding should parse"""
        code = """module c_bindings_mod
    interface
        function c_sqrt(x) bind(c, name="sqrt") result(res)
            use iso_c_binding, only: c_double
            real(c_double), value :: x
            real(c_double) :: res
        end function
    end interface
end module c_bindings_mod"""
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"C function with name binding should parse, got {errors}"
    
    def test_iso_c_binding_usage(self):
        """RED: ISO_C_BINDING module usage should parse"""
        code = """program c_interop_test
    use iso_c_binding, only: c_int, c_float, c_ptr, c_null_ptr
    implicit none
    
    integer(c_int) :: i
    real(c_float) :: x
    type(c_ptr) :: ptr
    
    ptr = c_null_ptr
end program c_interop_test"""
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"ISO_C_BINDING module usage should parse, got {errors}"
    
    def test_comprehensive_c_interop_example(self):
        """RED: Comprehensive C interop example should parse"""
        code = """module comprehensive_c_mod
    use iso_c_binding
    implicit none
    
    ! C interoperable type
    type, bind(c) :: point_t
        real(c_double) :: x, y
    end type point_t
    
    interface
        ! C function interface
        function c_distance(p1, p2) bind(c, name="distance") result(dist)
            import :: point_t, c_double
            type(point_t), value :: p1, p2
            real(c_double) :: dist
        end function
        
        ! C subroutine interface  
        subroutine c_print_point(p) bind(c, name="print_point")
            import :: point_t
            type(point_t), intent(in) :: p
        end subroutine
    end interface
    
contains
    subroutine test_interop()
        type(point_t) :: p1, p2
        real(c_double) :: distance
        
        p1%x = 1.0_c_double
        p1%y = 2.0_c_double
        p2%x = 4.0_c_double
        p2%y = 6.0_c_double
        
        distance = c_distance(p1, p2)
        call c_print_point(p1)
    end subroutine
end module comprehensive_c_mod"""
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"Comprehensive C interop should parse, got {errors}"

if __name__ == "__main__":
    pytest.main([__file__, "-v"])