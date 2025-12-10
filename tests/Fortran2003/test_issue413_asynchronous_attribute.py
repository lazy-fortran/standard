#!/usr/bin/env python3
"""
Test for Fortran 2003 ASYNCHRONOUS attribute (Issue #413).
Verifies that ASYNCHRONOUS is correctly parsed as an attr_spec (ISO/IEC 1539-1:2004 R503).
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


class TestAsynchronousAttribute:
    """Test Fortran 2003 ASYNCHRONOUS attribute parsing."""

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

    def test_asynchronous_simple(self):
        """Test simple ASYNCHRONOUS attribute on variable (ISO/IEC 1539-1:2004 Section 5.1.2.1)."""
        code = load_fixture(
            "Fortran2003",
            "test_issue413_asynchronous_attribute",
            "asynchronous_simple.f90",
        )
        tree, errors, exception = self.parse_fortran_code(code)
        assert errors == 0, f"Failed to parse asynchronous_simple.f90: {errors} errors"
        assert tree is not None, "Parse tree is None"

    def test_asynchronous_with_io(self):
        """Test ASYNCHRONOUS attribute with asynchronous I/O operations."""
        code = load_fixture(
            "Fortran2003",
            "test_issue413_asynchronous_attribute",
            "asynchronous_with_io.f90",
        )
        tree, errors, exception = self.parse_fortran_code(code)
        assert errors == 0, f"Failed to parse asynchronous_with_io.f90: {errors} errors"
        assert tree is not None, "Parse tree is None"

    def test_asynchronous_subroutine(self):
        """Test ASYNCHRONOUS attribute in subroutine arguments."""
        code = load_fixture(
            "Fortran2003",
            "test_issue413_asynchronous_attribute",
            "asynchronous_subroutine.f90",
        )
        tree, errors, exception = self.parse_fortran_code(code)
        assert errors == 0, f"Failed to parse asynchronous_subroutine.f90: {errors} errors"
        assert tree is not None, "Parse tree is None"

    def test_asynchronous_with_other_attributes(self):
        """Test ASYNCHRONOUS combined with other attributes."""
        code = """program test_async_combined
    implicit none
    integer, asynchronous, allocatable :: data_buffer
    integer, asynchronous, intent(inout) :: status_var
    real, asynchronous, target :: shared_data
end program test_async_combined
"""
        tree, errors, exception = self.parse_fortran_code(code)
        assert errors == 0, f"Failed to parse combined attributes code: {errors} errors"
        assert tree is not None, "Parse tree is None"


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
