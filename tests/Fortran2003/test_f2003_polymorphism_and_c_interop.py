#!/usr/bin/env python3
"""
Fortran 2003 advanced polymorphism and C interoperability tests (Issue #59).

This module adds focused tests for:
- SELECT TYPE constructs with CLASS(*) and nested type guards
- CLASS(*) declarations and unlimited polymorphic entities
- Additional C interoperability usage beyond existing tests:
  - BIND(C, NAME="...") on functions
  - Derived types with BIND(C)
  - IMPORT of C interop types in interface blocks
"""

import os
import sys
from pathlib import Path

import pytest
from antlr4 import InputStream, CommonTokenStream

sys.path.append(str(Path(__file__).parent.parent))
sys.path.append(str(Path(__file__).parent.parent.parent / "grammars/generated/modern"))

from Fortran2003Lexer import Fortran2003Lexer  # type: ignore
from Fortran2003Parser import Fortran2003Parser  # type: ignore
from fixture_utils import load_fixture


def parse_f2003(code: str):
    """Helper: parse Fortran 2003 code and return (tree, errors, parser)."""
    input_stream = InputStream(code)
    lexer = Fortran2003Lexer(input_stream)
    parser = Fortran2003Parser(CommonTokenStream(lexer))
    tree = parser.program_unit_f2003()
    return tree, parser.getNumberOfSyntaxErrors(), parser


class TestF2003Polymorphism:
    """Advanced polymorphism tests using existing F2003 grammar rules."""

    def test_nested_select_type_with_class_and_default(self):
        """
        Nested SELECT TYPE with TYPE IS, CLASS IS and CLASS DEFAULT.

        This exercises:
        - CLASS(*) selector in the outer SELECT TYPE
        - TYPE IS and CLASS IS guards
        - Nested SELECT TYPE inside a CLASS IS branch
        """
        code = load_fixture(
            "Fortran2003",
            "test_f2003_polymorphism_and_c_interop",
            "poly_nested_program.f90",
        )
        tree, errors, parser = parse_f2003(code)
        assert errors == 0, f"Nested SELECT TYPE construct failed with {errors} errors"
        assert tree is not None

    def test_select_type_with_integer_and_default(self):
        """
        Standard SELECT TYPE construct with TYPE IS and CLASS DEFAULT.

        This exercises the real Fortran 2003 spelling:
        - select type (obj)
        - type is (integer)
        - class default
        - end select
        """
        code = load_fixture(
            "Fortran2003",
            "test_f2003_polymorphism_and_c_interop",
            "poly_test_program.f90",
        )
        tree, errors, parser = parse_f2003(code)
        assert errors == 0, f"SELECT TYPE construct failed with {errors} errors"
        assert tree is not None

    def test_class_star_declaration(self):
        """
        CLASS(*) declarations without SELECT TYPE.

        This focuses on class_declaration_stmt and type_spec_or_star
        using the '*' (unlimited polymorphic) alternative.
        """
        code = load_fixture(
            "Fortran2003",
            "test_f2003_polymorphism_and_c_interop",
            "m_class_star_module.f90",
        )
        tree, errors, parser = parse_f2003(code)
        assert errors == 0, f"CLASS(*) declarations failed with {errors} errors"
        assert tree is not None


class TestF2003CInteropAdvanced:
    """Additional C interoperability coverage beyond existing tests."""

    def test_bind_c_name_on_function(self):
        """
        BIND(C, NAME="...") on a function.

        Existing tests cover BIND(C, NAME="...") on subroutines; this
        extends coverage to functions.
        """
        code = load_fixture(
            "Fortran2003",
            "test_f2003_polymorphism_and_c_interop",
            "c_funcs_module.f90",
        )
        tree, errors, parser = parse_f2003(code)
        assert errors == 0, f"BIND(C, NAME=...) on function failed with {errors} errors"
        assert tree is not None

    def test_derived_type_with_bind_c(self):
        """
        Derived type with BIND(C) and C interop component types.
        """
        code = load_fixture(
            "Fortran2003",
            "test_f2003_polymorphism_and_c_interop",
            "c_types_module.f90",
        )
        tree, errors, parser = parse_f2003(code)
        assert errors == 0, f"TYPE, BIND(C) failed with {errors} errors"
        assert tree is not None

    def test_import_c_interop_types_in_interface(self):
        """
        IMPORT of C interop types in an interface block.

        This uses import_stmt and import_name with c_interop_type.
        """
        code = load_fixture(
            "Fortran2003",
            "test_f2003_polymorphism_and_c_interop",
            "c_iface_module.f90",
        )
        tree, errors, parser = parse_f2003(code)
        assert errors == 0, f"IMPORT of C interop types failed with {errors} errors"
        assert tree is not None


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
