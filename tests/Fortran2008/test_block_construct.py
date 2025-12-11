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
from fixture_utils import load_fixture, find_contexts


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
        assert tree is not None, "Expected parse tree"

        blocks = find_contexts(
            tree, Fortran2008Parser.Block_construct_f2008Context
        )
        assert len(blocks) == 1, f"Expected 1 BLOCK construct, got {len(blocks)}"

        block_ctx = blocks[0]
        block_tokens = block_ctx.BLOCK()
        if isinstance(block_tokens, list):
            assert len(block_tokens) >= 2, "Expected BLOCK tokens for start/end"
        else:
            assert block_tokens is not None, "BLOCK token missing in construct"

        end_tokens = block_ctx.END()
        if isinstance(end_tokens, list):
            assert end_tokens, "END token missing in construct"
        else:
            assert end_tokens is not None, "END token missing in construct"

        newline_tokens = block_ctx.NEWLINE()
        if isinstance(newline_tokens, list):
            assert newline_tokens, "Expected NEWLINEs in BLOCK construct"
        else:
            assert newline_tokens is not None, "Expected NEWLINEs in BLOCK construct"

        spec_part = block_ctx.specification_part_f2008()
        assert spec_part is not None, "Expected local specification part inside BLOCK"
        decls = find_contexts(
            spec_part, Fortran2008Parser.Declaration_construct_f2008Context
        )
        assert decls, "Expected at least one declaration in BLOCK specification part"

        exec_part = block_ctx.execution_part_f2008()
        if exec_part is not None:
            executable_constructs = find_contexts(
                exec_part, Fortran2008Parser.Executable_construct_f2008Context
            )
            assert not executable_constructs, (
                "Did not expect executable statements in this snippet"
            )

    def test_minimal_block_construct_has_no_parts(self):
        code = (
            "program minimal_block\n"
            "block\n"
            "end block\n"
            "end program minimal_block\n"
        )
        tree, errors = self.parse_code(code)
        assert errors == 0, f"Expected no parse errors, got {errors}"
        blocks = find_contexts(
            tree, Fortran2008Parser.Block_construct_f2008Context
        )
        assert len(blocks) == 1
        block_ctx = blocks[0]

        block_tokens = block_ctx.BLOCK()
        if isinstance(block_tokens, list):
            assert len(block_tokens) >= 2, "Expected BLOCK tokens for start/end"
        else:
            assert block_tokens is not None, "BLOCK token missing in construct"

        end_tokens = block_ctx.END()
        if isinstance(end_tokens, list):
            assert end_tokens, "END token missing in construct"
        else:
            assert end_tokens is not None, "END token missing in construct"

        spec_part = block_ctx.specification_part_f2008()
        if spec_part is not None:
            decls = find_contexts(
                spec_part, Fortran2008Parser.Declaration_construct_f2008Context
            )
            assert not decls, "Did not expect declarations in minimal BLOCK"

        exec_part = block_ctx.execution_part_f2008()
        if exec_part is not None:
            executable_constructs = find_contexts(
                exec_part, Fortran2008Parser.Executable_construct_f2008Context
            )
            assert not executable_constructs, (
                "Did not expect executable statements in minimal BLOCK"
            )


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
