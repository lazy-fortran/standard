#!/usr/bin/env python3
"""
Fortran 2003 advanced polymorphism and C interoperability tests (Issue #59).

This module adds focused tests for:
- SELECT TYPE constructs with CLASS(*) and nested type guards
- CLASS(*) declarations and unlimited polymorphic entities
- Additional C interoperability usage beyond existing tests:
  - BIND(C, NAME="...") on functions
  - Derived types with BIND(C)
  - IMPORT of C interop types in interface blocks
"""

import os
import sys
from pathlib import Path

import pytest
from antlr4 import InputStream, CommonTokenStream

sys.path.append(str(Path(__file__).parent.parent.parent / "grammars"))

from Fortran2003Lexer import Fortran2003Lexer  # type: ignore
from Fortran2003Parser import Fortran2003Parser  # type: ignore


def parse_f2003(code: str):
    """Helper: parse Fortran 2003 code and return (tree, errors, parser)."""
    input_stream = InputStream(code)
    lexer = Fortran2003Lexer(input_stream)
    parser = Fortran2003Parser(CommonTokenStream(lexer))
    tree = parser.program_unit_f2003()
    return tree, parser.getNumberOfSyntaxErrors(), parser


class TestF2003Polymorphism:
    """Advanced polymorphism tests using existing F2003 grammar rules."""

    def test_select_type_with_integer_and_default(self):
        """
        Standard SELECT TYPE construct with TYPE IS and CLASS DEFAULT.

        This exercises the real Fortran 2003 spelling:
        - select type (obj)
        - type is (integer)
        - class default
        - end select
        """
        code = """program poly_test
  implicit none
  class(*), allocatable :: obj

  select type (obj)
  type is (integer)
    print *, 'int'
  class default
    print *, 'other'
  end select

end program poly_test
"""
        tree, errors, parser = parse_f2003(code)
        assert errors == 0, f"SELECT TYPE construct failed with {errors} errors"
        assert tree is not None

    def test_class_star_declaration(self):
        """
        CLASS(*) declarations without SELECT TYPE.

        This focuses on class_declaration_stmt and type_spec_or_star
        using the '*' (unlimited polymorphic) alternative.
        """
        code = """module m
    class(*), pointer :: p
    class(*), allocatable :: a(:)
end module m
"""
        tree, errors, parser = parse_f2003(code)
        assert errors == 0, f"CLASS(*) declarations failed with {errors} errors"
        assert tree is not None


class TestF2003CInteropAdvanced:
    """Additional C interoperability coverage beyond existing tests."""

    def test_bind_c_name_on_function(self):
        """
        BIND(C, NAME="...") on a function.

        Existing tests cover BIND(C, NAME="...") on subroutines; this
        extends coverage to functions.
        """
        code = """module c_funcs
    use iso_c_binding
    implicit none

contains

    integer(c_int) function add_ints(a, b) bind(c, name="add_ints")
        integer(c_int), value :: a, b
        add_ints = a + b
    end function add_ints

end module c_funcs
"""
        tree, errors, parser = parse_f2003(code)
        assert errors == 0, f"BIND(C, NAME=...) on function failed with {errors} errors"
        assert tree is not None

    def test_derived_type_with_bind_c(self):
        """
        Derived type with BIND(C) and C interop component types.
        """
        code = """module c_types
    use iso_c_binding
    implicit none

    type, bind(c) :: point
        real(c_double) :: x
        real(c_double) :: y
    end type point

end module c_types
"""
        tree, errors, parser = parse_f2003(code)
        assert errors == 0, f"TYPE, BIND(C) failed with {errors} errors"
        assert tree is not None

    def test_import_c_interop_types_in_interface(self):
        """
        IMPORT of C interop types in an interface block.

        This uses import_stmt and import_name with c_interop_type.
        """
        code = """module c_iface
    use iso_c_binding
    implicit none

    interface
        subroutine c_sub(x) bind(c)
            import :: c_int
            integer(c_int), value :: x
        end subroutine c_sub
    end interface

end module c_iface
"""
        tree, errors, parser = parse_f2003(code)
        assert errors == 0, f"IMPORT of C interop types failed with {errors} errors"
        assert tree is not None


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
