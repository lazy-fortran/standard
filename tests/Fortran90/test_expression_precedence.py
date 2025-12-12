#!/usr/bin/env python3
"""
Fortran 90 expression precedence and associativity tests (issue #678).

Validates that parse-tree structure reflects WG5 N692 / ISO/IEC 1539:1991:
- Section 7.1.1 (R703-R724)
- Section 7.4 and Table 7.7
"""

import sys
from pathlib import Path

import pytest


sys.path.append(str(Path(__file__).parent.parent))
from fixture_utils import find_contexts

sys.path.append(str(Path(__file__).parent.parent.parent / "grammars/generated/modern"))

try:
    from antlr4 import InputStream, CommonTokenStream
    from antlr4.error.ErrorListener import ErrorListener

    from Fortran90Lexer import Fortran90Lexer
    from Fortran90Parser import Fortran90Parser
except ImportError as e:
    pytest.skip(f"Fortran 90 grammar not built: {e}", allow_module_level=True)


class ParseErrorListener(ErrorListener):
    def __init__(self):
        super().__init__()
        self.errors: list[str] = []

    def syntaxError(self, recognizer, offendingSymbol, line, column, msg, e):
        self.errors.append(f"{line}:{column}: {msg}")


def _parse_expr(expr_text: str):
    input_stream = InputStream(expr_text)
    lexer = Fortran90Lexer(input_stream)
    stream = CommonTokenStream(lexer)
    parser = Fortran90Parser(stream)

    error_listener = ParseErrorListener()
    parser.removeErrorListeners()
    parser.addErrorListener(error_listener)

    tree = parser.expr_f90()
    if parser._input.LA(1) != -1:
        token = parser._input.LT(1)
        error_listener.errors.append(f"unconsumed token: {token.text}")
    return tree, error_listener.errors


def _contains_descendant(node, ctx_type) -> bool:
    for ctx in find_contexts(node, ctx_type):
        if ctx is not node:
            return True
    return False


class TestFortran90ExpressionPrecedence:
    def test_unary_binds_looser_than_power(self):
        tree, errors = _parse_expr("-A**2")
        assert errors == []

        unaries = find_contexts(tree, Fortran90Parser.UnaryExprF90Context)
        powers = find_contexts(tree, Fortran90Parser.PowerExprF90Context)
        assert unaries
        assert powers

        assert any(_contains_descendant(u, Fortran90Parser.PowerExprF90Context) for u in unaries)
        assert not any(_contains_descendant(p, Fortran90Parser.UnaryExprF90Context) for p in powers)

    def test_power_associates_right_to_left(self):
        tree, errors = _parse_expr("2**3**4")
        assert errors == []

        powers = find_contexts(tree, Fortran90Parser.PowerExprF90Context)
        assert len(powers) >= 2
        assert any(_contains_descendant(p, Fortran90Parser.PowerExprF90Context) for p in powers)

    def test_and_binds_tighter_than_or(self):
        tree, errors = _parse_expr("A .OR. B .AND. C")
        assert errors == []

        ors = find_contexts(tree, Fortran90Parser.LogicalOrExprF90Context)
        ands = find_contexts(tree, Fortran90Parser.LogicalAndExprF90Context)
        assert ors
        assert ands

        assert any(_contains_descendant(o, Fortran90Parser.LogicalAndExprF90Context) for o in ors)
        assert not any(_contains_descendant(a, Fortran90Parser.LogicalOrExprF90Context) for a in ands)

    def test_relational_binds_tighter_than_or(self):
        tree, errors = _parse_expr("L .OR. A + B .GE. C")
        assert errors == []

        ors = find_contexts(tree, Fortran90Parser.LogicalOrExprF90Context)
        gte = find_contexts(tree, Fortran90Parser.GreaterEqualExprF90Context)
        adds = find_contexts(tree, Fortran90Parser.AddSubExprF90Context)
        assert ors
        assert gte
        assert adds

        assert any(_contains_descendant(o, Fortran90Parser.GreaterEqualExprF90Context) for o in ors)
        assert any(_contains_descendant(g, Fortran90Parser.AddSubExprF90Context) for g in gte)

    def test_defined_unary_binds_tighter_than_power(self):
        tree, errors = _parse_expr(".foo. A**2")
        assert errors == []

        defined_unary = find_contexts(tree, Fortran90Parser.DefinedUnaryExprF90Context)
        powers = find_contexts(tree, Fortran90Parser.PowerExprF90Context)
        assert defined_unary
        assert powers

        assert any(
            _contains_descendant(p, Fortran90Parser.DefinedUnaryExprF90Context) for p in powers
        )
        assert not any(_contains_descendant(d, Fortran90Parser.PowerExprF90Context) for d in defined_unary)

    def test_defined_binary_has_lowest_precedence(self):
        tree, errors = _parse_expr("A .foo. B .AND. C")
        assert errors == []

        defined_binary = find_contexts(tree, Fortran90Parser.DefinedBinaryExprF90Context)
        ands = find_contexts(tree, Fortran90Parser.LogicalAndExprF90Context)
        assert defined_binary
        assert ands

        assert any(
            _contains_descendant(d, Fortran90Parser.LogicalAndExprF90Context)
            for d in defined_binary
        )
        assert not any(
            _contains_descendant(a, Fortran90Parser.DefinedBinaryExprF90Context) for a in ands
        )

    def test_concat_binds_looser_than_multiply(self):
        tree, errors = _parse_expr("A // B * C")
        assert errors == []

        concats = find_contexts(tree, Fortran90Parser.ConcatExprF90Context)
        mults = find_contexts(tree, Fortran90Parser.MultDivExprF90Context)
        assert concats
        assert mults

        assert any(_contains_descendant(c, Fortran90Parser.MultDivExprF90Context) for c in concats)

    def test_eqv_binds_looser_than_or(self):
        tree, errors = _parse_expr("A .EQV. B .OR. C")
        assert errors == []

        eqv = find_contexts(tree, Fortran90Parser.EquivalenceExprF90Context)
        ors = find_contexts(tree, Fortran90Parser.LogicalOrExprF90Context)
        assert eqv
        assert ors

        assert any(_contains_descendant(e, Fortran90Parser.LogicalOrExprF90Context) for e in eqv)
        assert not any(_contains_descendant(o, Fortran90Parser.EquivalenceExprF90Context) for o in ors)

    def test_relational_operator_is_non_associative(self):
        _, errors = _parse_expr("A < B < C")
        assert errors

    def test_consecutive_numeric_operators_rejected(self):
        _, errors = _parse_expr("A + -B")
        assert errors
