#!/usr/bin/env python3
"""REAL F2018 tests for FORMAT statement coverage"""

import sys
from pathlib import Path

sys.path.append(str(Path(__file__).parent.parent))
from fixture_utils import load_fixture

sys.path.append(str(Path(__file__).parent.parent.parent / "grammars/generated/modern"))

from antlr4 import CommonTokenStream, InputStream
from Fortran2018Lexer import Fortran2018Lexer
from Fortran2018Parser import Fortran2018Parser


class TestFortran2018FormatStatements:
    """Verify the F2018 parser accepts comprehensive FORMAT descriptors"""

    def parse_format(self, text):
        stream = InputStream(text + "\n")
        lexer = Fortran2018Lexer(stream)
        parser = Fortran2018Parser(CommonTokenStream(lexer))
        parser.format_stmt()
        return parser.getNumberOfSyntaxErrors()

    def test_format_complex_descriptors(self):
        code = load_fixture(
            "Fortran2018",
            "test_format_statement",
            "format_complex.f90",
        )

        for line in code.splitlines():
            stripped = line.strip()
            if not stripped:
                continue
            errors = self.parse_format(stripped)
            assert errors == 0, f"Grammar should accept FORMAT line '{stripped}', got {errors} errors"

    def test_format_unlimited_and_nested(self):
        code = load_fixture(
            "Fortran2018",
            "test_format_statement",
            "format_unlimited.f90",
        )

        errors = self.parse_format(code.strip())
        assert errors == 0, f"Timed unlimited FORMAT spec should parse without errors, got {errors}"
