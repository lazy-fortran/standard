#!/usr/bin/env python3
"""Verify that type parameter inquiries parse cleanly in F2018."""

import os
import sys
from pathlib import Path

from antlr4 import CommonTokenStream, InputStream

sys.path.append(str(Path(__file__).parent.parent))
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "../../grammars/generated/modern"))

from Fortran2018Lexer import Fortran2018Lexer  # noqa: E402
from Fortran2018Parser import Fortran2018Parser  # noqa: E402


def parse_f2018(code: str):
    """Return a parser after parsing the given F2018 code string."""

    input_stream = InputStream(code)
    lexer = Fortran2018Lexer(input_stream)
    parser = Fortran2018Parser(CommonTokenStream(lexer))
    parser.program_unit_f2018()
    return parser


def test_type_param_inquiry_expression_parses():
    """`designator % type-param-name` expressions parse without errors."""

    code = """PROGRAM type_param_inquiry
  IMPLICIT NONE

  TYPE :: t(k, n)
    INTEGER, KIND :: k = 0
    INTEGER, LEN :: n = 0
  END TYPE t

  TYPE(t(k=0, n=0)) :: sample
  INTEGER :: recorded_k, recorded_n

  recorded_k = sample%k
  recorded_n = sample%n
END PROGRAM type_param_inquiry"""

    parser = parse_f2018(code)
    assert parser.getNumberOfSyntaxErrors() == 0
