#!/usr/bin/env python3
"""
Issue #69 – Fortran 2003 SELECT TYPE and polymorphism semantics

This suite exercises the SELECT TYPE construct more systematically:

- Intrinsic, derived and CLASS(*) selectors
- Selector renames with `=>` in the SELECT TYPE header
- Multiple type-guard branches with and without selector names
- Nested SELECT TYPE blocks
- Clearly invalid guard forms that should produce syntax errors
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


class TestF2003SelectTypeSemantics:
    """Matrix of SELECT TYPE usage patterns."""

    def test_select_type_with_intrinsic_and_default(self):
        """SELECT TYPE with intrinsic TYPE IS and CLASS DEFAULT."""
        code = """
program select_intrinsic
  implicit none
  class(*), allocatable :: obj

  select type (obj)
  type is (integer)
     print *, 'int'
  type is (real)
     print *, 'real'
  class default
     print *, 'other'
  end select

end program select_intrinsic
"""
        tree, errors, _ = parse_f2003(code)
        assert tree is not None
        assert errors == 0

    def test_select_type_with_derived_and_selector_rename(self):
        """
        SELECT TYPE with derived type guards and selector rename.

        Exercises:
        - `select type (p => obj)`
        - TYPE IS (point_t) with and without a selector name
        - CLASS DEFAULT branch
        """
        code = """
module m_points
  implicit none

  type :: point_t
    real :: x, y
  end type point_t

contains

  subroutine handle_point(obj)
    class(*), intent(in) :: obj

    select type (p => obj)
    type is (point_t)
      print *, 'point:', p%x, p%y
    class default
      print *, 'not a point'
    end select

  end subroutine handle_point

end module m_points
"""
        tree, errors, _ = parse_f2003(code)
        assert tree is not None
        assert errors == 0

    def test_nested_select_type_with_class_star_and_renamed_selector(self):
        """
        Nested SELECT TYPE with CLASS(*) and renamed selector inside a branch.
        """
        code = """
program nested_select
  implicit none
  class(*), allocatable :: obj

  select type (obj)
  class is (shape_t)
    select type (s => obj)
    type is (circle_t)
      print *, 'circle branch'
    class default
      print *, 'shape but not circle'
    end select
  class default
    print *, 'non-shape'
  end select

end program nested_select
"""
        tree, errors, _ = parse_f2003(code)
        assert tree is not None
        assert errors == 0


class TestF2003SelectTypeNegative:
    """Negative tests for clearly invalid SELECT TYPE forms."""

    def test_invalid_guard_missing_is_keyword(self):
        """
        Guard missing the `is` keyword should not parse cleanly.

        Our grammar expects `TYPE is (type-spec)` / `CLASS is (type-spec)`.
        A form like `type (integer)` is therefore rejected.
        """
        code = """
program bad_guard
  implicit none
  class(*), allocatable :: obj

  select type (obj)
  type (integer)
     print *, 'missing is'
  class default
     print *, 'default'
  end select

end program bad_guard
"""
        _, errors, _ = parse_f2003(code)
        assert errors > 0

    def test_invalid_class_default_with_type_spec(self):
        """
        CLASS DEFAULT must not carry a type-spec; this malformed form
        should produce a syntax error under the current grammar.
        """
        code = """
program bad_default
  implicit none
  class(*), allocatable :: obj

  select type (obj)
  class default (integer)
     print *, 'illegal'
  end select

end program bad_default
"""
        _, errors, _ = parse_f2003(code)
        assert errors > 0

