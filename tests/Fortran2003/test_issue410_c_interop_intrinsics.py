#!/usr/bin/env python3
"""
Issue #410 – Fortran 2003 C interoperability intrinsic procedures

This module tests C interoperability intrinsic procedures as defined in
ISO/IEC 1539-1:2004 Section 15.2.5:
- C_LOC(X) – Returns C address of argument
- C_FUNLOC(X) – Returns C address of procedure argument
- C_ASSOCIATED(C_PTR_1 [, C_PTR_2]) – Check C pointer association status
- C_F_POINTER(CPTR, FPTR [, SHAPE]) – Convert C pointer to Fortran pointer
- C_F_PROCPOINTER(CPTR, FPTR) – Convert C function pointer to Fortran procedure pointer
"""

import sys
from pathlib import Path

import pytest
from antlr4 import InputStream, CommonTokenStream

sys.path.append(str(Path(__file__).parent.parent))
sys.path.append(str(Path(__file__).parent.parent.parent / "grammars/generated/modern"))

from Fortran2003Lexer import Fortran2003Lexer
from Fortran2003Parser import Fortran2003Parser


def parse_f2003(code: str):
    """Parse Fortran 2003 code and return (tree, errors, parser)."""
    input_stream = InputStream(code)
    lexer = Fortran2003Lexer(input_stream)
    parser = Fortran2003Parser(CommonTokenStream(lexer))
    tree = parser.program_unit_f2003()
    return tree, parser.getNumberOfSyntaxErrors(), parser


class TestF2003CInteropIntrinsics:
    """Test C interoperability intrinsic procedures."""

    def test_c_loc_basic(self):
        """C_LOC(X) returns C address of argument."""
        code = """
program test_c_loc
  use, intrinsic :: iso_c_binding
  implicit none
  integer(c_int), target :: i
  type(c_ptr) :: ptr

  ptr = c_loc(i)
end program test_c_loc
"""
        tree, errors, _ = parse_f2003(code)
        assert tree is not None
        assert errors == 0

    def test_c_funloc_basic(self):
        """C_FUNLOC(X) returns C address of procedure."""
        code = """
program test_c_funloc
  use, intrinsic :: iso_c_binding
  implicit none

  interface
    subroutine c_sub() bind(c)
    end subroutine
  end interface

  type(c_funptr) :: fptr
  fptr = c_funloc(c_sub)
end program test_c_funloc
"""
        tree, errors, _ = parse_f2003(code)
        assert tree is not None
        assert errors == 0

    def test_c_associated_single_pointer(self):
        """C_ASSOCIATED(C_PTR) checks if pointer is associated."""
        code = """
program test_c_associated
  use, intrinsic :: iso_c_binding
  implicit none
  type(c_ptr) :: ptr
  logical :: is_assoc

  is_assoc = c_associated(ptr)
end program test_c_associated
"""
        tree, errors, _ = parse_f2003(code)
        assert tree is not None
        assert errors == 0

    def test_c_associated_two_pointers(self):
        """C_ASSOCIATED(C_PTR_1, C_PTR_2) compares two pointers."""
        code = """
program test_c_associated_compare
  use, intrinsic :: iso_c_binding
  implicit none
  type(c_ptr) :: ptr1, ptr2
  logical :: same

  same = c_associated(ptr1, ptr2)
end program test_c_associated_compare
"""
        tree, errors, _ = parse_f2003(code)
        assert tree is not None
        assert errors == 0

    def test_c_f_pointer_basic(self):
        """C_F_POINTER(CPTR, FPTR) converts C pointer to Fortran pointer."""
        code = """
program test_c_f_pointer
  use, intrinsic :: iso_c_binding
  implicit none
  type(c_ptr) :: cptr
  integer(c_int), pointer :: fptr

  call c_f_pointer(cptr, fptr)
end program test_c_f_pointer
"""
        tree, errors, _ = parse_f2003(code)
        assert tree is not None
        assert errors == 0

    def test_c_f_pointer_with_shape(self):
        """C_F_POINTER with SHAPE specifies array dimensions."""
        code = """
program test_c_f_pointer_array
  use, intrinsic :: iso_c_binding
  implicit none
  type(c_ptr) :: cptr
  integer(c_int), pointer :: arr(:,:)
  integer(c_int) :: shape(2)

  shape = [10, 20]
  call c_f_pointer(cptr, arr, shape)
end program test_c_f_pointer_array
"""
        tree, errors, _ = parse_f2003(code)
        assert tree is not None
        assert errors == 0

    def test_c_f_procpointer_basic(self):
        """C_F_PROCPOINTER(CPTR, FPTR) converts C function pointer to Fortran."""
        code = """
program test_c_f_procpointer
  use, intrinsic :: iso_c_binding
  implicit none
  type(c_funptr) :: cfptr

  abstract interface
    subroutine sub()
    end subroutine
  end interface

  procedure(sub), pointer :: fsub
  call c_f_procpointer(cfptr, fsub)
end program test_c_f_procpointer
"""
        tree, errors, _ = parse_f2003(code)
        assert tree is not None
        assert errors == 0

    def test_all_c_interop_intrinsics_in_module(self):
        """Integration test: all C interop intrinsics in a module."""
        code = """
module c_interop_utils
  use, intrinsic :: iso_c_binding
  implicit none

contains

  function get_c_address(x) result(ptr)
    integer(c_int), target :: x
    type(c_ptr) :: ptr
    ptr = c_loc(x)
  end function get_c_address

  function check_pointer(ptr) result(is_assoc)
    type(c_ptr) :: ptr
    logical :: is_assoc
    is_assoc = c_associated(ptr)
  end function check_pointer

  subroutine convert_pointer(cptr, fptr)
    type(c_ptr) :: cptr
    integer(c_int), pointer :: fptr
    call c_f_pointer(cptr, fptr)
  end subroutine convert_pointer

  subroutine convert_procpointer(cfptr, fsub)
    type(c_funptr) :: cfptr
    abstract interface
      subroutine sub()
      end subroutine
    end interface
    procedure(sub), pointer :: fsub
    call c_f_procpointer(cfptr, fsub)
  end subroutine convert_procpointer

end module c_interop_utils
"""
        tree, errors, _ = parse_f2003(code)
        assert tree is not None
        assert errors == 0

    def test_c_interop_intrinsics_in_expressions(self):
        """Test C interop intrinsics used in expressions and assignments."""
        code = """
program test_c_interop_expressions
  use, intrinsic :: iso_c_binding
  implicit none

  integer(c_int), target :: x, y
  type(c_ptr) :: ptr_x, ptr_y
  logical :: ptrs_equal

  ptr_x = c_loc(x)
  ptr_y = c_loc(y)
  ptrs_equal = c_associated(ptr_x, ptr_y)

  if (c_associated(ptr_x)) then
    print *, 'Pointer is associated'
  end if

end program test_c_interop_expressions
"""
        tree, errors, _ = parse_f2003(code)
        assert tree is not None
        assert errors == 0

    def test_c_interop_in_interface_block(self):
        """Test C interop intrinsics in interface blocks."""
        code = """
program test_interface
  use, intrinsic :: iso_c_binding
  implicit none

  interface
    subroutine c_procedure(ptr) bind(c)
      use, intrinsic :: iso_c_binding
      type(c_ptr), value :: ptr
    end subroutine
  end interface

  integer(c_int), target :: x
  type(c_ptr) :: ptr

  ptr = c_loc(x)
  call c_procedure(ptr)

end program test_interface
"""
        tree, errors, _ = parse_f2003(code)
        assert tree is not None
        assert errors == 0
