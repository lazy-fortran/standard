#!/usr/bin/env python3
"""
Test suite for Fortran 2003 NON_INTRINSIC module nature (Issue #444).
Verifies that USE statements with NON_INTRINSIC keyword parse correctly.

Reference: ISO/IEC 1539-1:2004 Section 11.2.1 (R1109, R1110)
- R1109: use-stmt
- R1110: module-nature -> INTRINSIC | NON_INTRINSIC
"""

import sys
from pathlib import Path

# Ensure we can import shared test fixtures utilities
sys.path.append(str(Path(__file__).parent.parent))
from fixture_utils import load_fixture

# Add grammars directory to path
sys.path.append(str(Path(__file__).parent.parent.parent / "grammars/generated/modern"))

from antlr4 import *

class TestIssue444NonIntrinsic:
    """Test NON_INTRINSIC module nature in USE statements."""

    def parse_fortran_code(self, code, expect_success=True):
        """Helper to parse Fortran 2003 code."""
        try:
            from Fortran2003Lexer import Fortran2003Lexer
            from Fortran2003Parser import Fortran2003Parser

            input_stream = InputStream(code)
            lexer = Fortran2003Lexer(input_stream)
            parser = Fortran2003Parser(CommonTokenStream(lexer))

            tree = parser.program_unit_f2003()
            errors = parser.getNumberOfSyntaxErrors()

            if expect_success:
                assert errors == 0, f"Parse failed with {errors} errors"

            return tree, errors, None

        except Exception as e:
            if expect_success:
                assert False, f"Exception during parsing: {e}"
            return None, -1, str(e)

    def test_non_intrinsic_basic(self):
        """Test basic NON_INTRINSIC USE statement (ISO/IEC 1539-1:2004 R1110)."""
        code = load_fixture(
            "Fortran2003",
            "test_issue444_non_intrinsic_module_nature",
            "non_intrinsic_basic.f90",
        )
        self.parse_fortran_code(code)

    def test_non_intrinsic_with_only(self):
        """Test NON_INTRINSIC with ONLY clause (ISO/IEC 1539-1:2004 R1109)."""
        code = load_fixture(
            "Fortran2003",
            "test_issue444_non_intrinsic_module_nature",
            "non_intrinsic_with_only.f90",
        )
        self.parse_fortran_code(code)

    def test_mixed_module_natures(self):
        """Test mixing INTRINSIC and NON_INTRINSIC in same program."""
        code = load_fixture(
            "Fortran2003",
            "test_issue444_non_intrinsic_module_nature",
            "mixed_module_natures.f90",
        )
        self.parse_fortran_code(code)

    def test_non_intrinsic_direct_code(self):
        """Test NON_INTRINSIC with inline Fortran code."""
        code = """program test
    use, non_intrinsic :: my_mod
    implicit none
    print *, "Hello"
end program test
        """
        self.parse_fortran_code(code)

if __name__ == "__main__":
    import pytest
    pytest.main([__file__, "-v"])
