#!/usr/bin/env python3
"""
Issue #184 - Tighten SELECT TYPE guard syntax (TYPE IS / CLASS IS)

This test suite verifies that the SELECT TYPE guard syntax handles:
1. Case variations of the IS identifier (is, IS, Is, iS)
2. Varied spacing between TYPE/CLASS and IS
3. Correct behavior when 'is' is used as an identifier elsewhere
4. All valid guard forms (TYPE IS, CLASS IS, CLASS DEFAULT)

Per issue #184, the grammar treats IS as an IDENTIFIER token in type guards
rather than a dedicated keyword. This is a deliberate simplification that:
- Works correctly for all case variations (Fortran is case-insensitive)
- Handles varied whitespace naturally
- Allows 'is' to remain a valid variable name
- Follows the ANTLR best practice of minimal keyword reservation

ISO/IEC 1539-1:2004 Reference:
- R823: type-guard-stmt is TYPE IS ( type-spec ) [ select-construct-name ]
                        or CLASS IS ( derived-type-spec ) [ select-construct-name ]
                        or CLASS DEFAULT [ select-construct-name ]
"""

import sys
from pathlib import Path

import pytest
from antlr4 import InputStream, CommonTokenStream

sys.path.append(str(Path(__file__).parent.parent))
sys.path.append(str(Path(__file__).parent.parent.parent / "grammars"))

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


class TestTypeIsCaseVariations:
    """Test TYPE IS guards with different case patterns."""

    def test_type_is_case_variations(self):
        """TYPE IS should parse correctly with any case combination."""
        code = load_fixture(
            "Fortran2003",
            "test_issue184_select_type_guard_syntax",
            "type_is_case_variations.f90",
        )
        tree, errors, _ = parse_f2003(code)
        assert tree is not None
        assert errors == 0

    def test_class_is_case_variations(self):
        """CLASS IS should parse correctly with any case combination."""
        code = load_fixture(
            "Fortran2003",
            "test_issue184_select_type_guard_syntax",
            "class_is_case_variations.f90",
        )
        tree, errors, _ = parse_f2003(code)
        assert tree is not None
        assert errors == 0


class TestGuardSpacingVariations:
    """Test TYPE IS / CLASS IS guards with varied whitespace."""

    def test_spacing_variations(self):
        """Guards should parse with varied whitespace between keywords."""
        code = load_fixture(
            "Fortran2003",
            "test_issue184_select_type_guard_syntax",
            "spacing_variations.f90",
        )
        tree, errors, _ = parse_f2003(code)
        assert tree is not None
        assert errors == 0


class TestIsAsIdentifier:
    """Verify 'is' remains a valid identifier outside type guards."""

    def test_is_as_identifier(self):
        """Variable named 'is' should be allowed alongside TYPE IS guards."""
        code = load_fixture(
            "Fortran2003",
            "test_issue184_select_type_guard_syntax",
            "is_as_identifier.f90",
        )
        tree, errors, _ = parse_f2003(code)
        assert tree is not None
        assert errors == 0


class TestCompleteGuardMatrix:
    """Test all valid guard forms with realistic usage patterns."""

    def test_complete_guard_matrix(self):
        """All guard forms should parse in a realistic polymorphic program."""
        code = load_fixture(
            "Fortran2003",
            "test_issue184_select_type_guard_syntax",
            "complete_guard_matrix.f90",
        )
        tree, errors, _ = parse_f2003(code)
        assert tree is not None
        assert errors == 0


class TestInlineGuardCases:
    """Inline tests for specific edge cases without fixture files."""

    @pytest.mark.parametrize(
        "guard_line",
        [
            "type is (integer)",
            "type IS (integer)",
            "TYPE is (integer)",
            "TYPE IS (integer)",
            "Type Is (integer)",
            "TyPe iS (integer)",
        ],
    )
    def test_type_is_inline_cases(self, guard_line):
        """TYPE IS accepts any case combination for IS."""
        code = f"""program test
  implicit none
  class(*), allocatable :: obj
  select type (obj)
  {guard_line}
    print *, 'matched'
  end select
end program test
"""
        tree, errors, _ = parse_f2003(code)
        assert errors == 0

    @pytest.mark.parametrize(
        "guard_line",
        [
            "class is (base_t)",
            "class IS (base_t)",
            "CLASS is (base_t)",
            "CLASS IS (base_t)",
            "Class Is (base_t)",
            "ClAsS iS (base_t)",
        ],
    )
    def test_class_is_inline_cases(self, guard_line):
        """CLASS IS accepts any case combination for IS."""
        code = f"""module m
  type :: base_t
  end type
end module m
program test
  use m
  implicit none
  class(base_t), allocatable :: obj
  select type (obj)
  {guard_line}
    print *, 'matched'
  end select
end program test
"""
        tree, errors, _ = parse_f2003(code)
        assert errors == 0

    @pytest.mark.parametrize(
        "spacing",
        [
            "type is",
            "type  is",
            "type   is",
            "type    is",
            "type is ",
            "type is  ",
        ],
    )
    def test_varied_spacing_before_paren(self, spacing):
        """Guards accept varied spacing between keywords."""
        code = f"""program test
  implicit none
  class(*), allocatable :: obj
  select type (obj)
  {spacing}(integer)
    print *, 'matched'
  end select
end program test
"""
        tree, errors, _ = parse_f2003(code)
        assert errors == 0
