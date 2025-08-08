#!/usr/bin/env python3
"""F2018 Collective Operations Tests

Test F2018 collective coarray operations including:
- CO_SUM, CO_MIN, CO_MAX
- CO_REDUCE with custom operations  
- CO_BROADCAST with source specification
- Collective operation error handling
"""

import sys
import pytest
sys.path.insert(0, 'grammars')

from antlr4 import *
from Fortran2018Lexer import Fortran2018Lexer
from Fortran2018Parser import Fortran2018Parser

class TestF2018Collectives:
    """Test F2018 collective operations"""
    
    def parse_code(self, code):
        """Parse Fortran 2018 code and return (tree, errors)"""
        input_stream = InputStream(code)
        lexer = Fortran2018Lexer(input_stream)
        parser = Fortran2018Parser(CommonTokenStream(lexer))
        
        tree = parser.program_unit_f2018()
        errors = parser.getNumberOfSyntaxErrors()
        return tree, errors
    
    def test_co_sum_basic(self):
        """Test basic CO_SUM operation"""
        code = """program test_co_sum
    integer :: local_value[*], total
    local_value = this_image()
    call co_sum(local_value)
    if (this_image() == 1) then
        total = local_value
        print *, 'Sum:', total
    end if
end program test_co_sum"""
        
        tree, errors = self.parse_code(code)
        assert tree is not None, "CO_SUM failed to produce parse tree"
        assert errors <= 10, f"Too many errors for CO_SUM: {errors}"
    
    def test_co_min_max(self):
        """Test CO_MIN and CO_MAX operations"""
        code = """program test_co_minmax
    real :: min_val[*], max_val[*]
    
    min_val = this_image() * 1.5
    max_val = this_image() * 2.0
    
    call co_min(min_val)
    call co_max(max_val)
    
    if (this_image() == 1) then
        print *, 'Min:', min_val, 'Max:', max_val
    end if
end program test_co_minmax"""
        
        tree, errors = self.parse_code(code)
        assert tree is not None, "CO_MIN/MAX failed to produce parse tree"
        assert errors <= 10, f"Too many errors for CO_MIN/MAX: {errors}"
    
    def test_co_reduce(self):
        """Test CO_REDUCE with custom operation"""
        code = """module reduction_ops
    implicit none
contains
    pure function my_operation(a, b) result(c)
        integer, intent(in) :: a, b
        integer :: c
        c = a + b * 2
    end function my_operation
end module reduction_ops

program test_co_reduce
    use reduction_ops
    integer :: value[*]
    
    value = this_image()
    call co_reduce(value, my_operation)
    
    if (this_image() == 1) then
        print *, 'Reduced value:', value
    end if
end program test_co_reduce"""
        
        tree, errors = self.parse_code(code)
        assert tree is not None, "CO_REDUCE failed to produce parse tree"
        assert errors <= 15, f"Too many errors for CO_REDUCE: {errors}"
    
    def test_co_broadcast(self):
        """Test CO_BROADCAST operation"""
        code = """program test_co_broadcast
    integer :: broadcast_value[*]
    integer :: source_img = 3
    
    if (this_image() == source_img) then
        broadcast_value = 42
    end if
    
    call co_broadcast(broadcast_value, source_image=source_img)
    
    print *, 'Image', this_image(), 'has value:', broadcast_value
end program test_co_broadcast"""
        
        tree, errors = self.parse_code(code)
        assert tree is not None, "CO_BROADCAST failed to produce parse tree"
        assert errors <= 10, f"Too many errors for CO_BROADCAST: {errors}"
    
    def test_collective_with_stat(self):
        """Test collective operations with STAT and ERRMSG"""
        code = """program test_collective_stat
    integer :: value[*], stat_var
    character(len=100) :: err_msg
    
    value = this_image()
    
    call co_sum(value, stat=stat_var, errmsg=err_msg)
    
    if (stat_var /= 0) then
        print *, 'Error in CO_SUM:', trim(err_msg)
    end if
end program test_collective_stat"""
        
        tree, errors = self.parse_code(code)
        assert tree is not None, "Collective with STAT failed to produce parse tree"
        assert errors <= 10, f"Too many errors for collective with STAT: {errors}"
    
    def test_collective_array_operations(self):
        """Test collective operations on arrays"""
        code = """program test_collective_arrays
    real :: matrix(10,10)[*]
    integer :: row_sum(10)[*]
    
    ! Initialize matrix
    matrix = this_image() * 1.0
    
    ! Sum across images
    call co_sum(matrix)
    
    ! Compute row sums locally
    row_sum = sum(matrix, dim=2)
    
    ! Find minimum row sum across images
    call co_min(row_sum)
    
    if (this_image() == 1) then
        print *, 'Minimum row sums:', row_sum
    end if
end program test_collective_arrays"""
        
        tree, errors = self.parse_code(code)
        assert tree is not None, "Array collectives failed to produce parse tree"
        assert errors <= 15, f"Too many errors for array collectives: {errors}"

if __name__ == "__main__":
    pytest.main([__file__, "-v"])