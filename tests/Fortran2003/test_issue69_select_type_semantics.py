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


class TestF2003SelectTypeSemantics:
    """Matrix of SELECT TYPE usage patterns."""

    def test_select_type_with_intrinsic_and_default(self):
        """SELECT TYPE with intrinsic TYPE IS and CLASS DEFAULT."""
        code = load_fixture(
            "Fortran2003",
            "test_issue69_select_type_semantics",
            "select_intrinsic_program.f90",
        )
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
        code = load_fixture(
            "Fortran2003",
            "test_issue69_select_type_semantics",
            "m_points_module.f90",
        )
        tree, errors, _ = parse_f2003(code)
        assert tree is not None
        assert errors == 0

    def test_nested_select_type_with_class_star_and_renamed_selector(self):
        """
        Nested SELECT TYPE with CLASS(*) and renamed selector inside a branch.
        """
        code = load_fixture(
            "Fortran2003",
            "test_issue69_select_type_semantics",
            "nested_select_program.f90",
        )
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
        code = load_fixture(
            "Fortran2003",
            "test_issue69_select_type_semantics",
            "bad_guard_program.f90",
        )
        _, errors, _ = parse_f2003(code)
        assert errors > 0

    def test_invalid_class_default_with_type_spec(self):
        """
        CLASS DEFAULT must not carry a type-spec; this malformed form
        should produce a syntax error under the current grammar.
        """
        code = load_fixture(
            "Fortran2003",
            "test_issue69_select_type_semantics",
            "bad_default_program.f90",
        )
        _, errors, _ = parse_f2003(code)
        assert errors > 0
