#!/usr/bin/env python3
"""
Issue #68 – Fortran 2003 defined derived-type I/O (DT)

This suite focuses on the *syntax* that Fortran 2003 introduces for
defined derived-type I/O:

- Generic READ/WRITE interfaces such as READ(FORMATTED) / WRITE(UNFORMATTED)
- Type-bound GENERIC statements using the same generic-spec forms
- Use of DT edit descriptors inside explicit format strings

Note: The DT edit descriptor itself lives inside character format strings
and is therefore not parsed structurally by this grammar; we simply
verify that such formats are accepted as string literals in I/O control
lists.
"""

import sys
from pathlib import Path

import pytest
from antlr4 import CommonTokenStream, InputStream

# Add grammars directory to path
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


class TestFortran2003DefinedDerivedTypeIO:
    """Tests for Fortran 2003 defined derived-type I/O syntax."""

    def test_type_bound_generic_write_formatted(self):
        """Type-bound GENERIC :: WRITE(FORMATTED) => proc."""
        code = """
module defined_io_mod
  implicit none

  type :: point_t
    real :: x, y
  contains
    procedure :: write_formatted
    generic :: write(formatted) => write_formatted
  end type point_t

contains

  subroutine write_formatted(dtv, u, iotype, v_list, ios, msg)
    class(point_t), intent(in) :: dtv
    integer, intent(in)        :: u
    character(*), intent(in)   :: iotype
    integer, intent(in)        :: v_list(:)
    integer, intent(out)       :: ios
    character(*), intent(inout):: msg

    write(u, '(2F10.4)') dtv%x, dtv%y
        ios    = 0
        msg    = ''
  end subroutine write_formatted

end module defined_io_mod
"""
        tree, errors, _ = parse_f2003(code)
        assert tree is not None
        assert errors == 0

    def test_interface_write_formatted_and_unformatted(self):
        """INTERFACE WRITE(FORMATTED) / WRITE(UNFORMATTED)."""
        code = """
module interface_defined_io
  implicit none

  type :: point_t
    real :: x, y
  end type point_t

  interface write(formatted)
     subroutine write_point_formatted(dtv, u, iotype, v_list, ios, msg)
       import :: point_t
       class(point_t), intent(in) :: dtv
       integer, intent(in)        :: u
       character(*), intent(in)   :: iotype
       integer, intent(in)        :: v_list(:)
       integer, intent(out)       :: ios
       character(*), intent(inout):: msg
     end subroutine write_point_formatted
  end interface

  interface write(unformatted)
     subroutine write_point_unformatted(dtv, u, iotype, v_list, ios, msg)
       import :: point_t
       class(point_t), intent(in) :: dtv
       integer, intent(in)        :: u
       character(*), intent(in)   :: iotype
       integer, intent(in)        :: v_list(:)
       integer, intent(out)       :: ios
       character(*), intent(inout):: msg
     end subroutine write_point_unformatted
  end interface

end module interface_defined_io
"""
        tree, errors, _ = parse_f2003(code)
        assert tree is not None
        assert errors == 0

    def test_write_with_dt_edit_descriptor_in_format_string(self):
        """
        WRITE using an explicit format string that contains a DT edit descriptor.

        The DT edit descriptor syntax itself lives inside the character literal
        and is therefore not interpreted structurally by this grammar; we simply
        ensure that such a format is accepted as a string literal in the I/O
        control list.
        """
        code = """
program use_defined_io
  use defined_io_mod
  implicit none

  type(point_t) :: p

  p%x = 1.0
  p%y = 2.0

  ! DT edit descriptor inside format string (simplified example)
  write(*, '(DT\"point\"(10,2))') p
end program use_defined_io
"""
        tree, errors, _ = parse_f2003(code)
        assert tree is not None
        assert errors == 0

    def test_malformed_generic_dt_equals_is_rejected(self):
        """
        Malformed generic-spec using DT= inside READ/WRITE generic.

        Fortran 2003 defines generics READ(FORMATTED) / WRITE(UNFORMATTED);
        a form like WRITE(DT=...) is not valid and should result in a syntax
        error under this grammar.
        """
        code = """
module bad_dt_generic
  implicit none

  interface write(dt=point)
     module procedure write_point
  end interface

contains

  subroutine write_point(x)
    real, intent(in) :: x
  end subroutine write_point

end module bad_dt_generic
"""
        _, errors, _ = parse_f2003(code)
        assert errors > 0
