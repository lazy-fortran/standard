#!/usr/bin/env python3
"""Fortran 2028 grammar tests.

Focuses on WD-2028 template facility deltas on top of F2023 coverage.
"""

from __future__ import annotations

import os
import sys
from pathlib import Path

import pytest

# Add generated parser directory to import path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "../../grammars/generated/modern"))

try:
    from antlr4 import CommonTokenStream, InputStream, Token
    from Fortran2028Lexer import Fortran2028Lexer
    from Fortran2028Parser import Fortran2028Parser
    PARSER_AVAILABLE = True
except ImportError:
    PARSER_AVAILABLE = False
    Fortran2028Lexer = None
    Fortran2028Parser = None
    Token = None

FIXTURES_DIR = Path(__file__).parent.parent / "fixtures" / "Fortran2028"


def parse_program_unit(source: str):
    if not PARSER_AVAILABLE:
        pytest.skip("Fortran2028 parser not generated")
    lexer = Fortran2028Lexer(InputStream(source))
    parser = Fortran2028Parser(CommonTokenStream(lexer))
    return parser.program_unit_f2028(), parser


def tokenize(source: str):
    if not PARSER_AVAILABLE:
        pytest.skip("Fortran2028 lexer not generated")
    lexer = Fortran2028Lexer(InputStream(source))
    out = []
    while True:
        tok = lexer.nextToken()
        if tok.type == Token.EOF:
            break
        out.append(tok)
    return out


class TestFortran2028Lexer:
    def test_f2028_keywords(self):
        tokens = tokenize("template requirement require instantiate deferred constant")
        ttypes = [t.type for t in tokens]
        assert Fortran2028Lexer.TEMPLATE_KW in ttypes
        assert Fortran2028Lexer.REQUIREMENT_KW in ttypes
        assert Fortran2028Lexer.REQUIRE_KW in ttypes
        assert Fortran2028Lexer.INSTANTIATE_KW in ttypes
        assert Fortran2028Lexer.DEFERRED_KW in ttypes
        assert Fortran2028Lexer.CONSTANT_KW in ttypes


class TestFortran2028Parser:
    @pytest.mark.parametrize(
        "fixture",
        [
            "template_construct_wd.f90",
            "requirement_construct_wd.f90",
            "templated_function_wd.f90",
            "instantiate_stmt_wd.f90",
            "f2028_program_compat.f90",
        ],
    )
    def test_fixture_parses(self, fixture: str):
        source = (FIXTURES_DIR / fixture).read_text()
        tree, parser = parse_program_unit(source)
        assert tree is not None
        assert parser.getNumberOfSyntaxErrors() == 0


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
