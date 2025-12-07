#!/usr/bin/env python3
"""Issue #90 – Fortran 2003 PDT structure constructors and corner cases.

This suite exercises parameterized derived type (PDT) structure constructors
using the full F2003 syntax:

- derived-type-spec with type parameters followed by component values:
  type_name(k=..., n=...)(component_1, component_2, ...)
- Positional and keyword type parameters
- Positional and named component values
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
    """Parse Fortran 2003 code and return (tree, errors)."""
    input_stream = InputStream(code)
    lexer = Fortran2003Lexer(input_stream)
    parser = Fortran2003Parser(CommonTokenStream(lexer))
    tree = parser.program_unit_f2003()
    return tree, parser.getNumberOfSyntaxErrors()


class TestF2003PDTConstructors:
    """Extended PDT structure constructor coverage."""

    def test_pdt_constructor_with_keyword_type_param_and_component(self):
        """PDT constructor: keyword type param plus positional component."""
        code = load_fixture(
            "Fortran2003",
            "test_issue90_pdt_constructors",
            "pdt_t_module.f90",
        )
        tree, errors = parse_f2003(code)
        assert tree is not None
        assert errors == 0

    def test_pdt_constructor_with_mixed_type_params_and_components(self):
        """PDT constructor: mixed positional/keyword type params and components."""
        code = load_fixture(
            "Fortran2003",
            "test_issue90_pdt_constructors",
            "pdt_ctor_mixed_params_module.f90",
        )
        tree, errors = parse_f2003(code)
        assert tree is not None
        assert errors == 0

    def test_pdt_constructor_nested_in_component(self):
        """PDT constructor: nested constructors in component position."""
        code = load_fixture(
            "Fortran2003",
            "test_issue90_pdt_constructors",
            "pdt_ctor_nested_module.f90",
        )
        tree, errors = parse_f2003(code)
        assert tree is not None
        assert errors == 0

    def test_pdt_constructor_with_deferred_and_assumed_params_context(self):
        """PDT constructor: used alongside deferred and assumed type parameters."""
        code = load_fixture(
            "Fortran2003",
            "test_issue90_pdt_constructors",
            "pdt_ctor_poly_module.f90",
        )
        tree, errors = parse_f2003(code)
        assert tree is not None
        assert errors == 0

    def test_pdt_constructor_with_positional_type_params_and_components(self):
        """PDT constructor: positional type params and components."""
        code = load_fixture(
            "Fortran2003",
            "test_issue90_pdt_constructors",
            "pdt_matrix_ctor_module.f90",
        )
        tree, errors = parse_f2003(code)
        assert tree is not None
        assert errors == 0

    def test_pdt_constructor_with_array_components(self):
        """PDT constructor: array-valued components using F2003 [ ] syntax."""
        code = load_fixture(
            "Fortran2003",
            "test_issue90_pdt_constructors",
            "pdt_x_ctor_module.f90",
        )
        tree, errors = parse_f2003(code)
        assert tree is not None
        assert errors == 0
