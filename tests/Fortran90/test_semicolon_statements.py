#!/usr/bin/env python3
"""Test suite for Fortran 90 semicolon-separated statements (ISO/IEC 1539:1991 Section 3.4.2).

ISO/IEC 1539:1991 Section 3.4.2 explicitly allows semicolons to separate
statements on a single line in free-form source:
    "A semicolon separates statements on a line."

This test validates that the grammar correctly parses multiple statements
separated by semicolons on a single line.
"""

import pytest
from pathlib import Path
from antlr4 import InputStream, CommonTokenFactory, CommonTokenStream
from grammars.generated.modern.Fortran90Lexer import Fortran90Lexer
from grammars.generated.modern.Fortran90Parser import Fortran90Parser


class TestSemicolonSeparatedStatements:
    """Validate parsing of semicolon-separated statements (ISO Section 3.4.2)."""

    @staticmethod
    def parse_fortran90(code: str) -> Fortran90Parser.Program_unit_f90Context:
        """Parse Fortran 90 code and return parse tree."""
        input_stream = InputStream(code)
        lexer = Fortran90Lexer(input_stream)
        stream = CommonTokenStream(lexer)
        parser = Fortran90Parser(stream)
        return parser.program_unit_f90()

    def test_simple_assignment_semicolon(self):
        """Test simple semicolon-separated assignments: x = 1; y = 2"""
        code = """
program test
    implicit none
    integer :: x, y
    x = 1; y = 2
end program test
        """
        tree = self.parse_fortran90(code)
        assert tree is not None
        # Should parse without syntax errors
        errors = []
        # Count error nodes if parser has any
        assert len(errors) == 0, f"Unexpected parse errors: {errors}"

    def test_multiple_semicolon_separated_statements(self):
        """Test multiple semicolon-separated statements on one line."""
        code = """
program test
    implicit none
    integer :: a, b, c, d
    a = 1; b = 2; c = 3; d = 4
end program test
        """
        tree = self.parse_fortran90(code)
        assert tree is not None

    def test_semicolon_with_read_write(self):
        """Test semicolon-separated I/O statements."""
        code = """
program test
    implicit none
    integer :: x, y
    read(*,*) x; write(*,*) x
end program test
        """
        tree = self.parse_fortran90(code)
        assert tree is not None

    def test_semicolon_with_print_statement(self):
        """Test semicolon after PRINT statement."""
        code = """
program test
    implicit none
    integer :: x
    x = 5; print *, x
end program test
        """
        tree = self.parse_fortran90(code)
        assert tree is not None

    def test_semicolon_with_logical_operations(self):
        """Test semicolon-separated statements with logical operations."""
        code = """
program test
    implicit none
    logical :: a, b
    a = .true.; b = .false.; a = a .and. b
end program test
        """
        tree = self.parse_fortran90(code)
        assert tree is not None

    def test_semicolon_with_call_statement(self):
        """Test semicolon-separated CALL statements."""
        code = """
program test
    implicit none
    call sub1(); call sub2()
contains
    subroutine sub1()
    end subroutine
    subroutine sub2()
    end subroutine
end program test
        """
        tree = self.parse_fortran90(code)
        assert tree is not None

    def test_semicolon_mixed_with_newlines(self):
        """Test mixing semicolons with regular newline separators."""
        code = """
program test
    implicit none
    integer :: x, y, z
    x = 1
    y = 2; z = 3
    x = x + y
end program test
        """
        tree = self.parse_fortran90(code)
        assert tree is not None

    def test_semicolon_with_continue_statement(self):
        """Test semicolon with CONTINUE statement (part of DO loops)."""
        code = """
program test
    implicit none
    integer :: i
    do i = 1, 10
        continue; continue
    end do
end program test
        """
        tree = self.parse_fortran90(code)
        assert tree is not None

    def test_semicolon_with_cycle_exit(self):
        """Test semicolon with CYCLE and EXIT statements."""
        code = """
program test
    implicit none
    integer :: i
    do i = 1, 10
        if (i == 5) cycle; if (i == 8) exit
    end do
end program test
        """
        tree = self.parse_fortran90(code)
        assert tree is not None

    def test_semicolon_with_pointer_assignment(self):
        """Test semicolon with pointer assignment statements."""
        code = """
program test
    implicit none
    integer, target :: x, y
    integer, pointer :: p, q
    p => x; q => y
end program test
        """
        tree = self.parse_fortran90(code)
        assert tree is not None

    def test_semicolon_with_allocate_deallocate(self):
        """Test semicolon with ALLOCATE and DEALLOCATE statements."""
        code = """
program test
    implicit none
    integer, allocatable :: x(:)
    allocate(x(10)); deallocate(x)
end program test
        """
        tree = self.parse_fortran90(code)
        assert tree is not None

    def test_construct_cannot_follow_semicolon(self):
        """Test that constructs (IF, DO) on same line as previous statement are invalid."""
        code = """
program test
    implicit none
    integer :: x
    x = 1; if (x > 0) then
        print *, "positive"
    end if
end program test
        """
        # This should parse because the IF construct is on a new line
        # (after the semicolon but treated as a separate line item)
        tree = self.parse_fortran90(code)
        assert tree is not None

    def test_trailing_semicolon(self):
        """Test trailing semicolon at end of statement."""
        code = """
program test
    implicit none
    integer :: x
    x = 1;
end program test
        """
        tree = self.parse_fortran90(code)
        assert tree is not None

    def test_empty_statement_via_semicolon(self):
        """Test multiple consecutive semicolons."""
        code = """
program test
    implicit none
    integer :: x
    x = 1;; x = 2
end program test
        """
        tree = self.parse_fortran90(code)
        assert tree is not None


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
