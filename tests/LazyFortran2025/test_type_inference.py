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


def test_inferred_numeric_types():
    code = """
x = 3.14
y = 42
z = x * y
w = 2.71828
result = (x + w) / y
"""
    tree, errors = parse_lazy_fortran(code)
    assert errors == 0
    assert tree is not None


def test_inferred_string_types():
    code = """
name = "LazyFortran"
version = "2025"
full_name = name // " " // version
greeting = 'Hello, World!'
"""
    tree, errors = parse_lazy_fortran(code)
    assert errors == 0
    assert tree is not None


def test_inferred_array_types():
    code = """
numbers = [1, 2, 3, 4, 5]
doubles = numbers * 2
matrix = [[1, 2], [3, 4]]
sum_array = numbers + doubles
"""
    tree, errors = parse_lazy_fortran(code)
    assert errors == 0
    assert tree is not None


def test_mixed_inferred_and_declared():
    code = """
integer :: explicit_int = 10
x = 3.14
real :: explicit_real
y = 2 * explicit_int
explicit_real = x + y
z = explicit_real / 2.0
"""
    tree, errors = parse_lazy_fortran(code)
    assert errors == 0
    assert tree is not None


def test_inferred_in_loops():
    code = """
sum = 0.0
do i = 1, 100
    sum = sum + i * 0.5
end do

product = 1
do j = 1, 5
    product = product * j
end do
"""
    tree, errors = parse_lazy_fortran(code)
    assert errors == 0
    assert tree is not None


def test_inferred_function_returns():
    code = """
function calculate(a, b)
    calculate = a * b + 1.0
end function

result = calculate(3.0, 4.0)
"""
    tree, errors = parse_lazy_fortran(code)
    assert errors == 0
    assert tree is not None


def test_complex_type_inference():
    code = """
! Complex expressions with inferred types
alpha = 0.5
beta = 1.5
gamma = alpha * beta

! Array operations
vec1 = [1.0, 2.0, 3.0]
vec2 = [4.0, 5.0, 6.0]
dot_product = sum(vec1 * vec2)

! String operations
prefix = "Lazy"
suffix = "Fortran"
language = trim(prefix) // trim(suffix)
"""
    tree, errors = parse_lazy_fortran(code)
    assert errors == 0
    assert tree is not None