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


def test_bare_executable_statements():
    code = """
x = 3.14
y = 2.71
z = x + y
print *, 'Result:', z
"""
    tree, errors = parse_lazy_fortran(code)
    assert errors == 0
    assert tree is not None


def test_bare_module_like_code():
    code = """
integer :: counter = 0
real :: temperature = 25.0

subroutine increment()
    counter = counter + 1
end subroutine

function get_celsius(fahrenheit)
    real :: fahrenheit, get_celsius
    get_celsius = (fahrenheit - 32.0) * 5.0 / 9.0
end function
"""
    tree, errors = parse_lazy_fortran(code)
    assert errors == 0
    assert tree is not None


def test_mixed_declarations_and_statements():
    code = """
x = 1.0
integer :: i
y = 2.0
real :: z

do i = 1, 10
    z = x * i + y
    print *, i, z
end do
"""
    tree, errors = parse_lazy_fortran(code)
    assert errors == 0
    assert tree is not None


def test_backward_compatibility_with_program():
    code = """
program test_prog
    implicit none
    integer :: i
    real :: x
    
    x = 3.14
    do i = 1, 5
        print *, i, x * i
    end do
end program test_prog
"""
    tree, errors = parse_lazy_fortran(code)
    assert errors == 0
    assert tree is not None


def test_backward_compatibility_with_module():
    code = """
module math_utils
    implicit none
    real, parameter :: pi = 3.14159
    
contains
    function circle_area(radius)
        real :: radius, circle_area
        circle_area = pi * radius**2
    end function
end module math_utils
"""
    tree, errors = parse_lazy_fortran(code)
    assert errors == 0
    assert tree is not None