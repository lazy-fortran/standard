#!/usr/bin/env python3
"""
Issue #72 – Fortran 2003 fixed-form source with F2003 features

These tests exercise F2003 features written in traditional fixed-form
layout (upper-case, column-based style), to validate that the unified
lexer/parser handle both formats.
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


class TestF2003FixedFormFeatures:
    """Fixed-form tests for representative F2003 features."""

    def test_fixed_form_oop_and_class_star(self):
        """Fixed-form module with type-bound procedure and CLASS(*) dummy."""
        code = load_fixture(
            "Fortran2003",
            "test_issue72_fixed_form_f2003",
            "f2003_fixed_oop.f",
        )
        tree, errors, _ = parse_f2003(code)
        assert tree is not None
        assert errors == 0

    def test_fixed_form_select_type_and_associate(self):
        """SELECT TYPE and ASSOCIATE in fixed-form style."""
        code = load_fixture(
            "Fortran2003",
            "test_issue72_fixed_form_f2003",
            "f2003_fixed_select.f",
        )
        tree, errors, _ = parse_f2003(code)
        assert tree is not None
        assert errors == 0

    def test_fixed_form_pdt_and_bind_c(self):
        """PDT and BIND(C) usage in fixed-form."""
        code = load_fixture(
            "Fortran2003",
            "test_issue72_fixed_form_f2003",
            "f2003_fixed_pdt_c.f",
        )
        tree, errors, _ = parse_f2003(code)
        assert tree is not None
        assert errors == 0
