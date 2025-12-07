#!/usr/bin/env python3
"""
Issue #71 – Fortran 2003 parameterized derived types (PDTs) extended coverage

This suite exercises a broader set of PDT syntax and usage patterns:
- Kind and len parameters with positional and keyword arguments
- Deferred (:) and assumed (*) type parameters
- PDTs as components, arrays, dummy arguments and function results
- Simple structure constructors for PDTs
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


class TestF2003PDTExpanded:
    """Extended PDT tests covering more spec-allowed patterns."""

    def test_pdt_positional_and_keyword_instantiation(self):
        """PDT instantiation with positional and keyword parameters."""
        code = load_fixture(
            "Fortran2003",
            "test_issue71_pdt_extended",
            "pdt_matrix_module.f90",
        )
        tree, errors, _ = parse_f2003(code)
        assert tree is not None
        assert errors == 0

    def test_pdt_deferred_and_assumed_parameters(self):
        """PDTs using deferred (:) and assumed (*) type parameters."""
        code = load_fixture(
            "Fortran2003",
            "test_issue71_pdt_extended",
            "pdt_poly_module.f90",
        )
        tree, errors, _ = parse_f2003(code)
        assert tree is not None
        assert errors == 0

    def test_pdt_in_derived_types_and_procedures(self):
        """PDTs nested inside other types and used in procedures."""
        code = load_fixture(
            "Fortran2003",
            "test_issue71_pdt_extended",
            "pdt_usage_module.f90",
        )
        tree, errors, _ = parse_f2003(code)
        assert tree is not None
        assert errors == 0
