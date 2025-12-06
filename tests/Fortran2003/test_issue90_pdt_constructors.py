#!/usr/bin/env python3
"""Issue #90 – Fortran 2003 PDT structure constructors and corner cases.

This suite exercises parameterized derived type (PDT) structure constructors
using the full F2003 syntax:

- derived-type-spec with type parameters followed by component values:
  type_name(k=..., n=...)(component_1, component_2, ...)
- Positional and keyword type parameters
- Positional and named component values
"""

import sys
from pathlib import Path

import pytest
from antlr4 import InputStream, CommonTokenStream

sys.path.append(str(Path(__file__).parent.parent.parent / "grammars"))

from Fortran2003Lexer import Fortran2003Lexer
from Fortran2003Parser import Fortran2003Parser


def parse_f2003(code: str):
    """Parse Fortran 2003 code and return (tree, errors)."""
    input_stream = InputStream(code)
    lexer = Fortran2003Lexer(input_stream)
    parser = Fortran2003Parser(CommonTokenStream(lexer))
    tree = parser.program_unit_f2003()
    return tree, parser.getNumberOfSyntaxErrors()


class TestF2003PDTConstructors:
    """Extended PDT structure constructor coverage."""

    def test_pdt_constructor_with_keyword_type_param_and_component(self):
        """PDT constructor: keyword type param plus positional component."""
        code = """
module pdt_t_module
  implicit none

  type :: t(k)
    integer, kind :: k = 0
    integer :: value
  end type t

contains

  subroutine use_t
    type(t) :: foo
    ! Type parameter set with keyword, component by position
    foo = t(k=0)(42)
  end subroutine use_t

end module pdt_t_module
"""
        tree, errors = parse_f2003(code)
        assert tree is not None
        assert errors == 0

    def test_pdt_constructor_with_positional_type_params_and_components(self):
        """PDT constructor: positional type params and components."""
        code = """
module pdt_matrix_ctor
  implicit none

  type :: matrix_t(k, m, n)
    integer, kind :: k
    integer, len  :: m, n
    real(k)       :: data(m, n)
  end type matrix_t

contains

  subroutine build_matrix
    type(matrix_t(8,2,2)) :: a
    ! Positional type parameters followed by component values
    a = matrix_t(8,2,2)( [1.0, 2.0, 3.0, 4.0] )
  end subroutine build_matrix

end module pdt_matrix_ctor
"""
        tree, errors = parse_f2003(code)
        assert tree is not None
        assert errors == 0

    def test_pdt_constructor_with_array_components(self):
        """PDT constructor: array-valued components using F2003 [ ] syntax."""
        code = """
module pdt_x_ctor
  implicit none

  type :: x_t(n)
    integer, len :: n
    integer      :: v(n)
    character(len=8) :: label(n)
  end type x_t

contains

  subroutine init_x
    type(x_t(1)) :: a
    type(x_t(2)) :: b

    ! Structure constructors with array components
    a = x_t(1)( [1], ["a1"] )
    b = x_t(2)( [1,2], ["b1","b2"] )
  end subroutine init_x

end module pdt_x_ctor
"""
        tree, errors = parse_f2003(code)
        assert tree is not None
        assert errors == 0

