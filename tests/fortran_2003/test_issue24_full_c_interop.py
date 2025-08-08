#!/usr/bin/env python3
"""Complete F2003 C Interoperability Tests - Issue #24

Test-driven development for full C interoperability support including:
- BIND(C) syntax for procedures and types
- USE ISO_C_BINDING module support  
- Kind selectors with C types
- Complete procedure/type binding examples
"""

import sys
import pytest
sys.path.insert(0, 'grammars')

from antlr4 import *
from Fortran2003Lexer import Fortran2003Lexer
from Fortran2003Parser import Fortran2003Parser

class TestIssue24FullCInterop:
    """Complete C interoperability implementation tests"""
    
    def parse_code(self, code):
        """Parse Fortran 2003 code and return (tree, errors)"""
        input_stream = InputStream(code)
        lexer = Fortran2003Lexer(input_stream)
        parser = Fortran2003Parser(CommonTokenStream(lexer))
        
        tree = parser.program_unit_f2003()
        errors = parser.getNumberOfSyntaxErrors()
        return tree, errors
    
    def test_bind_c_subroutine_basic(self):
        """BIND(C) subroutine with basic syntax should parse"""
        code = """subroutine my_func() bind(c)
end subroutine my_func"""
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"BIND(C) subroutine should parse, got {errors}"
    
    def test_bind_c_subroutine_with_name(self):
        """BIND(C) subroutine with explicit name should parse"""
        code = """subroutine fortran_func() bind(c, name="my_func")
end subroutine fortran_func"""
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"BIND(C) with name should parse, got {errors}"
    
    def test_bind_c_function(self):
        """BIND(C) function should parse"""
        code = """function add(a, b) result(sum) bind(c)
    integer(c_int), value :: a, b
    integer(c_int) :: sum
end function add"""
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"BIND(C) function should parse, got {errors}"
    
    def test_bind_c_derived_type(self):
        """BIND(C) derived type should parse"""
        code = """type, bind(c) :: my_struct
    integer(c_int) :: x
    real(c_float) :: y
end type my_struct"""
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"BIND(C) derived type should parse, got {errors}"
    
    def test_use_iso_c_binding(self):
        """USE ISO_C_BINDING should parse"""
        code = """module example
    use iso_c_binding
    implicit none
end module example"""
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"USE ISO_C_BINDING should parse, got {errors}"
    
    def test_use_iso_c_binding_only(self):
        """USE ISO_C_BINDING with ONLY should parse"""
        code = """module example
    use iso_c_binding, only: c_int, c_float, c_ptr
    implicit none
end module example"""
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"USE ISO_C_BINDING with ONLY should parse, got {errors}"
    
    def test_integer_kind_c_int(self):
        """Integer with C kind selector should parse"""
        code = """module example
    integer(c_int) :: x
    integer(kind=c_int) :: y
end module example"""
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"integer(c_int) should parse, got {errors}"
    
    def test_real_kind_c_float(self):
        """Real with C kind selector should parse"""
        code = """module example
    real(c_float) :: x
    real(kind=c_double) :: y
end module example"""
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"real(c_float) should parse, got {errors}"
    
    def test_procedure_with_value_attribute(self):
        """Procedure arguments with VALUE attribute should parse"""
        code = """subroutine my_proc(x, y) bind(c)
    integer(c_int), value :: x
    real(c_float), value :: y
end subroutine my_proc"""
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"VALUE attribute in C procedure should parse, got {errors}"
    
    def test_comprehensive_c_interop_example(self):
        """Complete C interop example with all features"""
        code = """module c_interface
    use iso_c_binding
    implicit none
    
    type, bind(c) :: point
        real(c_double) :: x, y
    end type point
    
    interface
        function my_distance(p1, p2) result(dist) bind(c, name="distance")
            import :: c_double
            type(point), value :: p1, p2
            real(c_double) :: dist
        end function my_distance
    end interface
    
contains
    
    subroutine init_point(p, x_val, y_val) bind(c)
        type(point), intent(out) :: p
        real(c_double), value :: x_val, y_val
        p%x = x_val
        p%y = y_val
    end subroutine init_point
    
end module c_interface"""
        
        tree, errors = self.parse_code(code)
        assert errors == 0, f"Comprehensive C interop example should parse, got {errors}"

if __name__ == "__main__":
    pytest.main([__file__, "-v"])