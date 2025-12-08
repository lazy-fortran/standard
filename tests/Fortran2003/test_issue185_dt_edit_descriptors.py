#!/usr/bin/env python3
"""
Issue #185 – Fortran 2003 DT edit descriptors in FORMAT strings

This suite verifies that FORMAT strings containing DT edit descriptors
(ISO/IEC 1539-1:2004 Section 10.2.2) are accepted by the grammar.

DT edit descriptors have the form: DT [ char-literal-constant ] [ ( v-list ) ]
where:
  - char-literal-constant is an optional type-name string
  - v-list is an optional comma-separated list of integer values

IMPORTANT: The grammar intentionally treats FORMAT strings as opaque character
literals. DT edit descriptor syntax within FORMAT strings is NOT parsed
structurally; the descriptor is simply part of the character literal content.
This is a deliberate design choice documented in:
  - docs/fortran_2003_audit.md (Section 7, Enhanced I/O)
  - grammars/Fortran2003Parser.g4 (f2003_io_spec rule comments)

The tests confirm that FORMAT strings with DT descriptors are accepted as valid
character literals in I/O control lists, which is the expected behavior.
"""

import sys
from pathlib import Path

import pytest
from antlr4 import CommonTokenStream, InputStream

sys.path.append(str(Path(__file__).parent.parent))
sys.path.append(str(Path(__file__).parent.parent.parent / "grammars/generated/modern"))

from Fortran2003Lexer import Fortran2003Lexer
from Fortran2003Parser import Fortran2003Parser
from fixture_utils import load_fixture


def parse_f2003(code: str):
    """Parse Fortran 2003 code and return (tree, errors, parser)."""
    input_stream = InputStream(code)
    lexer = Fortran2003Lexer(input_stream)
    parser = Fortran2003Parser(CommonTokenStream(lexer))
    tree = parser.program_unit_f2003()
    return tree, parser.getNumberOfSyntaxErrors(), parser


class TestFortran2003DTEditDescriptors:
    """
    Tests for DT edit descriptors in FORMAT strings.

    Per ISO/IEC 1539-1:2004 Section 10.2.2, DT edit descriptors enable
    defined derived-type I/O. The forms tested include:
      - DT (basic form)
      - DT"typename" (with type-name string)
      - DT(v1,v2,...) (with v-list)
      - DT"typename"(v1,v2,...) (with both)
    """

    def test_dt_edit_inline_format(self):
        """
        DT edit descriptors in inline format specifications.

        Tests various DT descriptor forms within inline format strings
        passed directly to WRITE statements.
        """
        code = load_fixture(
            "Fortran2003",
            "test_issue185_dt_edit_descriptors",
            "dt_edit_inline_format.f90",
        )
        tree, errors, _ = parse_f2003(code)
        assert tree is not None
        assert errors == 0

    def test_dt_edit_format_stmt(self):
        """
        DT edit descriptors in labeled FORMAT statements.

        Tests DT descriptors used in separate FORMAT statements that
        are referenced by label in WRITE statements.
        """
        code = load_fixture(
            "Fortran2003",
            "test_issue185_dt_edit_descriptors",
            "dt_edit_format_stmt.f90",
        )
        tree, errors, _ = parse_f2003(code)
        assert tree is not None
        assert errors == 0

    def test_dt_edit_read_write(self):
        """
        DT edit descriptors in READ and WRITE statements.

        Tests DT descriptors in both READ and WRITE operations,
        including file I/O contexts.
        """
        code = load_fixture(
            "Fortran2003",
            "test_issue185_dt_edit_descriptors",
            "dt_edit_read_write.f90",
        )
        tree, errors, _ = parse_f2003(code)
        assert tree is not None
        assert errors == 0

    def test_dt_edit_interface_block(self):
        """
        DT edit descriptors with interface-block defined I/O.

        Tests DT descriptors used with defined I/O established via
        interface blocks (as opposed to type-bound procedures).
        """
        code = load_fixture(
            "Fortran2003",
            "test_issue185_dt_edit_descriptors",
            "dt_edit_interface_block.f90",
        )
        tree, errors, _ = parse_f2003(code)
        assert tree is not None
        assert errors == 0

    def test_dt_basic_inline(self):
        """Basic DT edit descriptor in inline format."""
        code = """
program test_dt
  implicit none
  type :: t_t
    integer :: x
  end type t_t
  type(t_t) :: v
  write(*, '(DT)') v
end program test_dt
"""
        tree, errors, _ = parse_f2003(code)
        assert tree is not None
        assert errors == 0

    def test_dt_with_typename_double_quote(self):
        """DT with type-name using double quotes."""
        code = """
program test_dt
  implicit none
  type :: t_t
    integer :: x
  end type t_t
  type(t_t) :: v
  write(*, '(DT"mytype")') v
end program test_dt
"""
        tree, errors, _ = parse_f2003(code)
        assert tree is not None
        assert errors == 0

    def test_dt_with_typename_single_quote(self):
        """DT with type-name using single quotes (alternate delimiter)."""
        code = '''
program test_dt
  implicit none
  type :: t_t
    integer :: x
  end type t_t
  type(t_t) :: v
  write(*, "(DT'mytype')") v
end program test_dt
'''
        tree, errors, _ = parse_f2003(code)
        assert tree is not None
        assert errors == 0

    def test_dt_with_vlist(self):
        """DT with v-list parameters."""
        code = """
program test_dt
  implicit none
  type :: t_t
    integer :: x
  end type t_t
  type(t_t) :: v
  write(*, '(DT(10,2))') v
end program test_dt
"""
        tree, errors, _ = parse_f2003(code)
        assert tree is not None
        assert errors == 0

    def test_dt_with_typename_and_vlist(self):
        """DT with both type-name and v-list."""
        code = """
program test_dt
  implicit none
  type :: t_t
    integer :: x
  end type t_t
  type(t_t) :: v
  write(*, '(DT"fmt"(5,3,1))') v
end program test_dt
"""
        tree, errors, _ = parse_f2003(code)
        assert tree is not None
        assert errors == 0

    def test_dt_multiple_in_format(self):
        """Multiple DT descriptors in one format string."""
        code = """
program test_dt
  implicit none
  type :: t_t
    integer :: x
  end type t_t
  type(t_t) :: v1, v2
  write(*, '(DT,1X,DT)') v1, v2
end program test_dt
"""
        tree, errors, _ = parse_f2003(code)
        assert tree is not None
        assert errors == 0

    def test_dt_with_repeat_count(self):
        """DT with repeat count prefix."""
        code = """
program test_dt
  implicit none
  type :: t_t
    integer :: x
  end type t_t
  type(t_t) :: v1, v2
  write(*, '(2DT)') v1, v2
end program test_dt
"""
        tree, errors, _ = parse_f2003(code)
        assert tree is not None
        assert errors == 0

    def test_dt_in_read_statement(self):
        """DT in a READ statement format."""
        code = """
program test_dt
  implicit none
  type :: t_t
    integer :: x
  end type t_t
  type(t_t) :: v
  integer :: ios
  read(10, '(DT)', iostat=ios) v
end program test_dt
"""
        tree, errors, _ = parse_f2003(code)
        assert tree is not None
        assert errors == 0

    def test_dt_mixed_with_regular_descriptors(self):
        """DT mixed with regular edit descriptors."""
        code = """
program test_dt
  implicit none
  type :: t_t
    integer :: x
  end type t_t
  type(t_t) :: v
  write(*, '(A,DT"val"(10),I5,DT,F8.2)') 'x=', v, 42, v, 3.14d0
end program test_dt
"""
        tree, errors, _ = parse_f2003(code)
        assert tree is not None
        assert errors == 0
