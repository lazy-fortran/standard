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


def test_all_features_combined():
    code = """
! LazyFortran2025 - all features combined
! No program block, implicit none default, type inference

pi = 3.14159
radius = 5.0
area = pi * radius**2

print *, 'Circle area:', area

! Procedures without contains
subroutine calculate_volume(height)
    volume = area * height
    print *, 'Cylinder volume:', volume
end subroutine

function sphere_volume()
    sphere_volume = (4.0/3.0) * pi * radius**3
end function

! Use the procedures
call calculate_volume(10.0)
sphere_vol = sphere_volume()
print *, 'Sphere volume:', sphere_vol
"""
    tree, errors = parse_lazy_fortran(code)
    assert errors == 0
    assert tree is not None


def test_real_world_example():
    code = """
! Scientific computation without boilerplate
temperature_c = 25.0
temperature_f = temperature_c * 9.0/5.0 + 32.0

data = [1.2, 3.4, 5.6, 7.8, 9.0]
mean = sum(data) / size(data)
squared_diffs = (data - mean)**2
variance = sum(squared_diffs) / size(data)
std_dev = sqrt(variance)

print *, 'Statistics:'
print *, 'Mean:', mean
print *, 'Std Dev:', std_dev

subroutine report(label, value)
    print *, label, ':', value
end subroutine

call report('Temperature (F)', temperature_f)
"""
    tree, errors = parse_lazy_fortran(code)
    assert errors == 0
    assert tree is not None


def test_nested_structures():
    code = """
! Complex nested structures
counter = 0

subroutine outer()
    local_var = 100
    
    subroutine inner()
        counter = counter + 1
        local_var = local_var + counter
    end subroutine
    
    call inner()
    print *, local_var
end subroutine

do i = 1, 3
    call outer()
end do
"""
    tree, errors = parse_lazy_fortran(code)
    assert errors == 0
    assert tree is not None


def test_modern_style_coding():
    code = """
! Modern, concise LazyFortran2025 style
fibonacci = [1, 1]

do n = 3, 10
    next_fib = fibonacci(n-1) + fibonacci(n-2)
    fibonacci = [fibonacci, next_fib]
end do

print *, 'Fibonacci sequence:', fibonacci

function factorial(n)
    if (n <= 1) then
        factorial = 1
    else
        factorial = n * factorial(n-1)
    end if
end function

factorials = [(factorial(i), i=1,5)]
print *, 'Factorials:', factorials
"""
    tree, errors = parse_lazy_fortran(code)
    assert errors == 0
    assert tree is not None


def test_edge_cases():
    code = """
! Edge cases and special scenarios

! Single line
x = 42

! Empty procedures
subroutine empty_sub()
end subroutine

function empty_func()
end function

! Minimal module
module minimal
end module

! Mixed old and new style
program optional_end
    y = 3.14
    
    subroutine nested()
        print *, y
    end subroutine
    
    call nested()
! Note: end program is optional in LazyFortran2025
"""
    tree, errors = parse_lazy_fortran(code)
    assert errors == 0
    assert tree is not None