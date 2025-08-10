import sys
import os
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '../../grammars')))

import pytest
from antlr4 import *
from LazyFortran2025Lexer import LazyFortran2025Lexer
from LazyFortran2025Parser import LazyFortran2025Parser


def parse_lazy_fortran(code):
    input_stream = InputStream(code)
    lexer = LazyFortran2025Lexer(input_stream)
    stream = CommonTokenStream(lexer)
    parser = LazyFortran2025Parser(stream)
    tree = parser.fortran_program()
    return tree, parser.getNumberOfSyntaxErrors()


def test_procedures_without_contains():
    code = """
x = 10.0
y = 20.0

subroutine print_sum()
    print *, 'Sum:', x + y
end subroutine

function multiply()
    real :: multiply
    multiply = x * y
end function
"""
    tree, errors = parse_lazy_fortran(code)
    assert errors == 0
    assert tree is not None


def test_module_without_contains():
    code = """
module utilities
    real :: global_var = 1.0
    
    subroutine reset()
        global_var = 0.0
    end subroutine
    
    function get_value()
        real :: get_value
        get_value = global_var
    end function
end module
"""
    tree, errors = parse_lazy_fortran(code)
    assert errors == 0
    assert tree is not None


def test_bare_code_with_procedures():
    code = """
! Main calculations
result = 0.0

subroutine update_result(value)
    real :: value
    result = result + value
end subroutine

function compute(a, b)
    real :: a, b, compute
    compute = a * b + result
end function

! Use the procedures
call update_result(5.0)
final = compute(2.0, 3.0)
"""
    tree, errors = parse_lazy_fortran(code)
    assert errors == 0
    assert tree is not None


def test_backward_compatibility_with_contains():
    code = """
program test
    implicit none
    real :: x = 1.0
    
contains
    subroutine show()
        print *, x
    end subroutine
end program
"""
    tree, errors = parse_lazy_fortran(code)
    assert errors == 0
    assert tree is not None


def test_mixed_contains_and_no_contains():
    code = """
! Top level without program block
x = 5.0

subroutine direct_sub()
    print *, 'Direct'
end subroutine

! Nested module with contains
module nested
    integer :: n
contains
    subroutine nested_sub()
        n = 10
    end subroutine
end module
"""
    tree, errors = parse_lazy_fortran(code)
    assert errors == 0
    assert tree is not None