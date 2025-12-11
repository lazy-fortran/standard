#!/usr/bin/env python3
"""F2008 BLOCK construct tests.

Validates that standalone BLOCK/END BLOCK constructs are accepted starting in
Fortran 2008 (ISO/IEC 1539-1:2010 Section 8.1.4).
"""

import sys
import pytest
from pathlib import Path

sys.path.insert(0, "grammars/generated/modern")
sys.path.append(str(Path(__file__).parent.parent))

from antlr4 import *
from Fortran2008Lexer import Fortran2008Lexer
from Fortran2008Parser import Fortran2008Parser
from fixture_utils import load_fixture


class TestBlockConstructF2008:
    """Test F2008 BLOCK construct parsing and structure."""

    def parse_code(self, code):
        input_stream = InputStream(code)
        lexer = Fortran2008Lexer(input_stream)
        parser = Fortran2008Parser(CommonTokenStream(lexer))
        tree = parser.program_unit_f2008()
        errors = parser.getNumberOfSyntaxErrors()
        return tree, errors

    def test_block_fixtures_parse(self):
        fixtures = [
            "block_construct.f90",
            "minimal_block_construct.f90",
            "block_with_local_var.f90",
        ]

        for fixture_name in fixtures:
            code = load_fixture(
                "Fortran2008",
                "test_block_construct",
                fixture_name,
            )
            tree, errors = self.parse_code(code)
            assert tree is not None, f"{fixture_name} did not produce parse tree"
            assert errors == 0, f"{fixture_name}: expected 0 errors, got {errors}"

    def test_block_construct_structure(self):
        code = (
            "program test_block_snippet\n"
            "block\n"
            "integer :: local_var\n"
            "end block\n"
            "end program test_block_snippet\n"
        )
        tree, errors = self.parse_code(code)
        assert errors == 0, f"Expected no parse errors, got {errors}"
        tree_text = str(tree.getText()) if tree else ""
        assert "block" in tree_text.lower()
        assert "end" in tree_text.lower()


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
