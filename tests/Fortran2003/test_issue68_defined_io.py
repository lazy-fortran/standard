#!/usr/bin/env python3
"""
Issue #68 – Fortran 2003 defined derived-type I/O (DT)

This suite focuses on the *syntax* that Fortran 2003 introduces for
defined derived-type I/O:

- Generic READ/WRITE interfaces such as READ(FORMATTED) / WRITE(UNFORMATTED)
- Type-bound GENERIC statements using the same generic-spec forms
- Use of DT edit descriptors inside explicit format strings

Note: The DT edit descriptor itself lives inside character format strings
and is therefore not parsed structurally by this grammar; we simply
verify that such formats are accepted as string literals in I/O control
lists.
"""

import sys
from pathlib import Path

import pytest
from antlr4 import CommonTokenStream, InputStream

sys.path.append(str(Path(__file__).parent.parent))
# Add grammars directory to path
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


class TestFortran2003DefinedDerivedTypeIO:
    """Tests for Fortran 2003 defined derived-type I/O syntax."""

    def test_type_bound_generic_write_formatted(self):
        """Type-bound GENERIC :: WRITE(FORMATTED) => proc."""
        code = load_fixture(
            "Fortran2003",
            "test_issue68_defined_io",
            "defined_io_mod.f90",
        )
        tree, errors, _ = parse_f2003(code)
        assert tree is not None
        assert errors == 0

    def test_interface_write_formatted_and_unformatted(self):
        """INTERFACE WRITE(FORMATTED) / WRITE(UNFORMATTED)."""
        code = load_fixture(
            "Fortran2003",
            "test_issue68_defined_io",
            "interface_defined_io.f90",
        )
        tree, errors, _ = parse_f2003(code)
        assert tree is not None
        assert errors == 0

    def test_write_with_dt_edit_descriptor_in_format_string(self):
        """
        WRITE using an explicit format string that contains a DT edit descriptor.

        The DT edit descriptor syntax itself lives inside the character literal
        and is therefore not interpreted structurally by this grammar; we simply
        ensure that such a format is accepted as a string literal in the I/O
        control list.
        """
        code = load_fixture(
            "Fortran2003",
            "test_issue68_defined_io",
            "use_defined_io_program.f90",
        )
        tree, errors, _ = parse_f2003(code)
        assert tree is not None
        assert errors == 0

    def test_malformed_generic_dt_equals_is_rejected(self):
        """
        Malformed generic-spec using DT= inside READ/WRITE generic.

        Fortran 2003 defines generics READ(FORMATTED) / WRITE(UNFORMATTED);
        a form like WRITE(DT=...) is not valid and should result in a syntax
        error under this grammar.
        """
        code = load_fixture(
            "Fortran2003",
            "test_issue68_defined_io",
            "bad_dt_generic_module.f90",
        )
        _, errors, _ = parse_f2003(code)
        assert errors > 0
