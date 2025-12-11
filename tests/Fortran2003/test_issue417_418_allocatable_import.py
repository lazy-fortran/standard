#!/usr/bin/env python3
"""
Test suite for Fortran 2003 ALLOCATABLE enhancements (Issue #417) and
IMPORT statement forms (Issue #418).

Issue #417: ALLOCATABLE attribute enhancements (ISO/IEC 1539-1:2004 Sections 5.1.2.4, 5.2.3, 12.3.2.1)
- ALLOCATABLE dummy arguments (R518)
- ALLOCATABLE function results (R1225)
- ALLOCATABLE derived type components (R426)

Issue #418: Extended IMPORT statement forms (ISO/IEC 1539-1:2004 R1209)
- IMPORT (without args - import all)
- IMPORT :: name-list (explicit list with double colon)
- IMPORT name-list (explicit list without double colon)
"""

import sys
import pytest
from pathlib import Path

# Add tests root (for fixture_utils) and grammars directory to path
ROOT = Path(__file__).resolve().parent.parent
sys.path.append(str(ROOT))
sys.path.append(str(ROOT.parent / "grammars/generated/modern"))

from fixture_utils import load_fixture

from antlr4 import *


class TestAllocatableEnhancements:
    """Test Fortran 2003 ALLOCATABLE attribute enhancements (Issue #417)."""

    def parse_fortran_code(self, code, expect_success=True):
        """Helper to parse Fortran 2003 code."""
        try:
            from Fortran2003Lexer import Fortran2003Lexer
            from Fortran2003Parser import Fortran2003Parser

            input_stream = InputStream(code)
            lexer = Fortran2003Lexer(input_stream)
            parser = Fortran2003Parser(CommonTokenStream(lexer))

            # Parse starting with F2003 program unit
            tree = parser.program_unit_f2003()
            errors = parser.getNumberOfSyntaxErrors()

            if expect_success:
                assert errors == 0, f"Parse failed with {errors} errors"
                assert tree is not None, "Parse tree is None"

            return tree, errors, None

        except Exception as e:
            if expect_success:
                assert False, f"Exception during parsing: {e}"
            return None, -1, str(e)

    def test_allocatable_component_in_derived_type(self):
        """
        Test ALLOCATABLE components in derived types (ISO/IEC 1539-1:2004 Section 5.2.3, R426).

        F2003 allows:
            type :: container_t
              real, allocatable :: data(:)
              integer, allocatable :: indices(:)
            end type
        """
        code = """module allocatable_components
  implicit none

  type :: container_t
    real, allocatable :: data(:)
    integer, allocatable :: indices(:)
    character(len=80), allocatable :: label
  end type container_t

  type :: nested_t
    type(container_t), allocatable :: containers(:)
  end type nested_t

end module allocatable_components
"""
        tree, errors, exception = self.parse_fortran_code(code)
        assert errors == 0, f"Failed to parse allocatable components: {errors} errors"
        assert tree is not None, "Parse tree is None"

    def test_allocatable_dummy_arguments(self):
        """
        Test ALLOCATABLE dummy arguments (ISO/IEC 1539-1:2004 Section 12.3.2.1, R518).

        F2003 allows:
            subroutine process(buffer)
              real, allocatable, intent(inout) :: buffer(:)
            end subroutine
        """
        code = """module allocatable_dummies
  implicit none

contains

  subroutine process_buffer(buffer)
    real, allocatable, intent(inout) :: buffer(:)
    if (.not. allocated(buffer)) then
      allocate(buffer(100))
    end if
  end subroutine process_buffer

  subroutine init_array(arr, n)
    integer, intent(in) :: n
    real, allocatable, intent(out) :: arr(:)
    allocate(arr(n))
  end subroutine init_array

  subroutine read_data(data_array)
    integer, allocatable, intent(in) :: data_array(:)
    ! Process read-only allocatable argument
  end subroutine read_data

end module allocatable_dummies
"""
        tree, errors, exception = self.parse_fortran_code(code)
        assert errors == 0, f"Failed to parse allocatable dummies: {errors} errors"
        assert tree is not None, "Parse tree is None"

    def test_allocatable_function_results(self):
        """
        Test ALLOCATABLE function results (ISO/IEC 1539-1:2004 Section 5.1.2.4, R1225).

        F2003 allows:
            function create_array(n) result(arr)
              integer, intent(in) :: n
              real, allocatable :: arr(:)
            end function
        """
        code = """module allocatable_results
  implicit none

contains

  function create_array(n) result(arr)
    integer, intent(in) :: n
    real, allocatable :: arr(:)
    allocate(arr(n))
    arr = 0.0
  end function create_array

  function load_data(filename) result(data)
    character(len=*), intent(in) :: filename
    real, allocatable :: data(:,:)
    integer :: rows, cols
    ! Read dimensions and allocate
    rows = 100
    cols = 50
    allocate(data(rows, cols))
  end function load_data

  function get_sparse_vector() result(vec)
    real, allocatable :: vec(:)
    allocate(vec(1000))
  end function get_sparse_vector

end module allocatable_results
"""
        tree, errors, exception = self.parse_fortran_code(code)
        assert errors == 0, f"Failed to parse allocatable results: {errors} errors"
        assert tree is not None, "Parse tree is None"

    def test_allocatable_combined_with_other_attributes(self):
        """
        Test ALLOCATABLE combined with other F2003 attributes.
        ALLOCATABLE can appear with INTENT, TARGET, etc. in dummy arguments.
        """
        code = """module allocatable_combined
  implicit none

contains

  subroutine test_combination(arr1, arr2)
    real, allocatable, intent(inout) :: arr1(:)
    integer, allocatable, intent(out) :: arr2(:)

    allocate(arr1(100))
    allocate(arr2(50))
  end subroutine test_combination

  subroutine test_target(arr_tgt)
    real, allocatable, target, intent(inout) :: arr_tgt(:)
    allocate(arr_tgt(100))
  end subroutine test_target

end module allocatable_combined
"""
        tree, errors, exception = self.parse_fortran_code(code)
        assert errors == 0, f"Failed to parse allocatable with combined attributes: {errors} errors"
        assert tree is not None, "Parse tree is None"

    def test_allocatable_multidimensional(self):
        """Test ALLOCATABLE with various dimension specifications."""
        code = """module allocatable_multidim
  implicit none

contains

  subroutine process_matrix(mat)
    real, allocatable, intent(inout) :: mat(:,:)
    allocate(mat(100, 50))
  end subroutine process_matrix

  subroutine process_tensor(tens)
    real, allocatable, intent(inout) :: tens(:,:,:)
    allocate(tens(10, 20, 30))
  end subroutine process_tensor

  function create_jagged_array() result(arr)
    real, allocatable :: arr(:)
    allocate(arr(1000))
  end function create_jagged_array

end module allocatable_multidim
"""
        tree, errors, exception = self.parse_fortran_code(code)
        assert errors == 0, f"Failed to parse multidimensional allocatable: {errors} errors"
        assert tree is not None, "Parse tree is None"


class TestImportStatementForms:
    """Test Fortran 2003 IMPORT statement forms (Issue #418)."""

    def parse_fortran_code(self, code, expect_success=True):
        """Helper to parse Fortran 2003 code."""
        try:
            from Fortran2003Lexer import Fortran2003Lexer
            from Fortran2003Parser import Fortran2003Parser

            input_stream = InputStream(code)
            lexer = Fortran2003Lexer(input_stream)
            parser = Fortran2003Parser(CommonTokenStream(lexer))

            # Parse starting with F2003 program unit
            tree = parser.program_unit_f2003()
            errors = parser.getNumberOfSyntaxErrors()

            if expect_success:
                assert errors == 0, f"Parse failed with {errors} errors"
                assert tree is not None, "Parse tree is None"

            return tree, errors, None

        except Exception as e:
            if expect_success:
                assert False, f"Exception during parsing: {e}"
            return None, -1, str(e)

    def test_import_all_forms(self):
        """
        Test IMPORT statement without args (ISO/IEC 1539-1:2004 R1209).

        IMPORT (without name list) means import all accessible names from host.
        """
        code = """module test_import_all
  implicit none

  integer, parameter :: my_kind = selected_real_kind(15)
  real :: shared_value = 1.0

  interface
    subroutine external_sub(x)
      import
      real(my_kind), intent(in) :: x
    end subroutine external_sub
  end interface

end module test_import_all
"""
        tree, errors, exception = self.parse_fortran_code(code)
        assert errors == 0, f"Failed to parse IMPORT all: {errors} errors"
        assert tree is not None, "Parse tree is None"

    def test_import_with_double_colon(self):
        """
        Test IMPORT with explicit double colon and name list (ISO/IEC 1539-1:2004 R1209).

        IMPORT :: name-list is the most explicit form with optional double colon.
        """
        code = """module test_import_explicit
  implicit none

  integer, parameter :: kind_val = selected_real_kind(15)
  type :: my_type
    real(kind_val) :: value
  end type my_type

  interface
    subroutine process_explicit(x, y)
      import :: kind_val, my_type
      real(kind_val), intent(in) :: x
      type(my_type), intent(out) :: y
    end subroutine process_explicit
  end interface

end module test_import_explicit
"""
        tree, errors, exception = self.parse_fortran_code(code)
        assert errors == 0, f"Failed to parse IMPORT with :: and names: {errors} errors"
        assert tree is not None, "Parse tree is None"

    def test_import_multiple_names(self):
        """
        Test IMPORT with multiple names in explicit list.
        """
        code = """module test_import_multiple
  implicit none

  integer, parameter :: dp = selected_real_kind(15)
  integer, parameter :: max_size = 1000
  type :: config_t
    integer :: size
  end type config_t

  interface
    subroutine read_config(cfg)
      import :: dp, max_size, config_t
      type(config_t), intent(out) :: cfg
    end subroutine read_config
  end interface

end module test_import_multiple
"""
        tree, errors, exception = self.parse_fortran_code(code)
        assert errors == 0, f"Failed to parse IMPORT with multiple names: {errors} errors"
        assert tree is not None, "Parse tree is None"


    def test_import_in_interface_block(self):
        """Test IMPORT statement within interface blocks (most common usage)."""
        code = """module test_import_interface
  implicit none

  integer, parameter :: kind_val = selected_real_kind(15)
  real :: global_param = 1.0

  interface
    function external_func(x) result(y)
      import
      real(kind_val), intent(in) :: x
      real(kind_val) :: y
    end function external_func

    subroutine external_sub(a, b)
      import :: kind_val, global_param
      real(kind_val), intent(in) :: a
      real, intent(out) :: b
    end subroutine external_sub
  end interface

end module test_import_interface
"""
        tree, errors, exception = self.parse_fortran_code(code)
        assert errors == 0, f"Failed to parse IMPORT in interface: {errors} errors"
        assert tree is not None, "Parse tree is None"

    def test_import_with_various_host_entities(self):
        """
        Test IMPORT can reference various entity types from host scope.

        Host scope may contain:
        - Named constants (parameters)
        - Variables
        - Derived types
        """
        code = """module test_import_entities
  implicit none

  integer, parameter :: buffer_size = 1024
  real :: tolerance = 1.0e-10

  type :: state_t
    integer :: status
    real :: value
  end type state_t

  interface
    subroutine interface_sub(state_arg)
      import :: buffer_size, tolerance, state_t
      type(state_t), intent(inout) :: state_arg
    end subroutine interface_sub
  end interface

end module test_import_entities
"""
        tree, errors, exception = self.parse_fortran_code(code)
        assert errors == 0, f"Failed to parse IMPORT with various entities: {errors} errors"
        assert tree is not None, "Parse tree is None"

    def test_import_without_implicit_none(self):
        """Test IMPORT works with and without IMPLICIT NONE."""
        code = """module test_import_implicit

  integer :: some_param = 100

  interface
    subroutine sub_with_implicit(x)
      import :: some_param
      integer, intent(in) :: x
    end subroutine sub_with_implicit
  end interface

end module test_import_implicit
"""
        tree, errors, exception = self.parse_fortran_code(code)
        assert errors == 0, f"Failed to parse IMPORT without IMPLICIT NONE: {errors} errors"
        assert tree is not None, "Parse tree is None"


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
