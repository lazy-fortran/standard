#!/usr/bin/env python3
"""
Issue #70 – Fortran 2003 full C interoperability syntax coverage (extended tests)

This module adds focused syntax tests around:
- BIND(C) on functions and subroutines (with and without NAME=)
- BIND(C) on derived types with various interoperable component types
- VALUE + OPTIONAL attributes in interoperable dummy arguments
- IMPORT of C interop types in interface blocks
- Negative cases for clearly invalid BIND forms
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


class TestF2003CInteropExtended:
    """Additional positive coverage for Fortran 2003 C interoperability syntax."""

    def test_bind_c_on_function_and_subroutine_with_name(self):
        """BIND(C) with and without NAME= on functions and subroutines."""
        code = load_fixture(
            "Fortran2003",
            "test_issue70_c_interop_extended",
            "c_api_module.f90",
        )
        tree, errors, _ = parse_f2003(code)
        assert tree is not None
        assert errors == 0

    def test_bind_c_derived_type_with_various_c_components(self):
        """Derived type with BIND(C) and interoperable components."""
        code = load_fixture(
            "Fortran2003",
            "test_issue70_c_interop_extended",
            "c_structs_module.f90",
        )
        tree, errors, _ = parse_f2003(code)
        assert tree is not None
        assert errors == 0

    def test_value_and_optional_attributes_in_c_binding(self):
        """VALUE and OPTIONAL attributes in interoperable dummies (simplified)."""
        code = load_fixture(
            "Fortran2003",
            "test_issue70_c_interop_extended",
            "c_optional_args_module.f90",
        )
        tree, errors, _ = parse_f2003(code)
        assert tree is not None
        assert errors == 0

    def test_import_c_types_in_interface_block(self):
        """IMPORT of multiple C interop types inside an interface block."""
        code = load_fixture(
            "Fortran2003",
            "test_issue70_c_interop_extended",
            "c_interfaces_module.f90",
        )
        tree, errors, _ = parse_f2003(code)
        assert tree is not None
        assert errors == 0


class TestF2003CInteropNegative:
    """Negative tests for obviously invalid C interoperability usage."""

    def test_invalid_bind_language_and_missing_name_string(self):
        """Reject non-C language and malformed NAME clauses."""
        cases = [
            # Unsupported language identifier in BIND
            load_fixture(
                "Fortran2003",
                "test_issue70_c_interop_extended",
                "bad_lang_module.f90",
            ),
            # NAME without string literal
            load_fixture(
                "Fortran2003",
                "test_issue70_c_interop_extended",
                "bad_name_module.f90",
            ),
        ]

        for code in cases:
            _, errors, _ = parse_f2003(code)
            assert errors > 0
