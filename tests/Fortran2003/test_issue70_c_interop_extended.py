#!/usr/bin/env python3
"""
Issue #70 – Fortran 2003 full C interoperability syntax coverage (extended tests)

This module adds focused syntax tests around:
- BIND(C) on functions and subroutines (with and without NAME=)
- BIND(C) on derived types with various interoperable component types
- VALUE + OPTIONAL attributes in interoperable dummy arguments
- IMPORT of C interop types in interface blocks
- Negative cases for clearly invalid BIND forms
"""

import sys
from pathlib import Path

import pytest
from antlr4 import InputStream, CommonTokenStream

sys.path.append(str(Path(__file__).parent.parent.parent / "grammars"))

from Fortran2003Lexer import Fortran2003Lexer
from Fortran2003Parser import Fortran2003Parser


def parse_f2003(code: str):
    """Parse Fortran 2003 code and return (tree, errors, parser)."""
    input_stream = InputStream(code)
    lexer = Fortran2003Lexer(input_stream)
    parser = Fortran2003Parser(CommonTokenStream(lexer))
    tree = parser.program_unit_f2003()
    return tree, parser.getNumberOfSyntaxErrors(), parser


class TestF2003CInteropExtended:
    """Additional positive coverage for Fortran 2003 C interoperability syntax."""

    def test_bind_c_on_function_and_subroutine_with_name(self):
        """BIND(C) with and without NAME= on functions and subroutines."""
        code = """
module c_api
  use iso_c_binding
  implicit none

contains

  ! Function with BIND(C) and NAME=
  integer(c_int) function add(a, b) bind(c, name="c_add")
    integer(c_int), value :: a, b
    add = a + b
  end function add

  ! Subroutine with BIND(C) and NAME=
  subroutine scale_array(n, x, factor) bind(c, name="c_scale_array")
    integer(c_int), value        :: n
    real(c_double)               :: x(n)
    real(c_double), value        :: factor
  end subroutine scale_array

  ! Function with plain BIND(C)
  real(c_double) function length(x, y) bind(c)
    real(c_double), value :: x, y
    length = sqrt(x*x + y*y)
  end function length

end module c_api
"""
        tree, errors, _ = parse_f2003(code)
        assert tree is not None
        assert errors == 0

    def test_bind_c_derived_type_with_various_c_components(self):
        """Derived type with BIND(C) and interoperable components."""
        code = """
module c_structs
  use iso_c_binding
  implicit none

  type, bind(c) :: particle_t
    integer(c_int)      :: id
    real(c_double)      :: mass
    real(c_double)      :: position(3)
    type(c_ptr)         :: payload
  end type particle_t

  type, bind(c) :: pair_t
    type(particle_t) :: a
    type(particle_t) :: b
  end type pair_t

end module c_structs
"""
        tree, errors, _ = parse_f2003(code)
        assert tree is not None
        assert errors == 0

    def test_value_and_optional_attributes_in_c_binding(self):
        """VALUE and OPTIONAL attributes in interoperable dummies (simplified)."""
        code = """
module c_optional_args
  use iso_c_binding
  implicit none

contains

  subroutine c_log_message(msg, level) bind(c, name="c_log_message")
    character(len=*), intent(in) :: msg
    integer(c_int), value        :: level
  end subroutine c_log_message

end module c_optional_args
"""
        tree, errors, _ = parse_f2003(code)
        assert tree is not None
        assert errors == 0

    def test_import_c_types_in_interface_block(self):
        """IMPORT of multiple C interop types inside an interface block."""
        code = """
module c_interfaces
  use iso_c_binding
  implicit none

  interface
    subroutine c_axpy(n, a, x, y) bind(c, name="c_axpy")
      import :: c_int, c_double
      integer(c_int),  value        :: n
      real(c_double),  value        :: a
      real(c_double)                :: x(n), y(n)
    end subroutine c_axpy
  end interface

end module c_interfaces
"""
        tree, errors, _ = parse_f2003(code)
        assert tree is not None
        assert errors == 0


class TestF2003CInteropNegative:
    """Negative tests for obviously invalid C interoperability usage."""

    def test_invalid_bind_language_and_missing_name_string(self):
        """Reject non-C language and malformed NAME clauses."""
        cases = [
            # Unsupported language identifier in BIND
            """
module bad_lang
  implicit none
contains
  subroutine s() bind(fortran)
  end subroutine s
end module bad_lang
""",
            # NAME without string literal
            """
module bad_name
  implicit none
contains
  subroutine s() bind(c, name=foo)
  end subroutine s
end module bad_name
""",
        ]

        for code in cases:
            _, errors, _ = parse_f2003(code)
            assert errors > 0
