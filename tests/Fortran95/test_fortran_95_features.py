#!/usr/bin/env python3
"""
Fortran 95 (1995) Feature Tests

Minimal test suite validating a subset of the F95-specific extensions
implemented in Fortran95Lexer.g4 and Fortran95Parser.g4.

Focus (currently exercised):
- FORALL constructs and statements
- Enhanced WHERE / ELSEWHERE constructs
- PURE / ELEMENTAL procedure headers
- F95 array constructors and intrinsics
"""

import os
import sys
import pytest

# Add grammars directory to Python path for generated parsers
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '../../grammars'))

try:
    from antlr4 import InputStream, CommonTokenStream
    from Fortran95Lexer import Fortran95Lexer
    from Fortran95Parser import Fortran95Parser
except ImportError as e:
    pytest.skip(f"Fortran 95 grammar not built: {e}", allow_module_level=True)


class TestFortran95Lexer:
    """Test F95 lexer extensions on top of F90."""

    def create_lexer(self, text: str) -> Fortran95Lexer:
        return Fortran95Lexer(InputStream(text))

    def get_tokens(self, text: str):
        lexer = self.create_lexer(text)
        tokens = []
        while True:
            token = lexer.nextToken()
            if token.type == -1:
                break
            tokens.append(token)
        return tokens

    def test_forall_keywords(self):
        """FORALL and END FORALL should be recognized."""
        tokens = self.get_tokens("FORALL (i = 1:10)")
        assert any(t.type == Fortran95Lexer.FORALL for t in tokens)

        tokens = self.get_tokens("END FORALL")
        assert any(t.type == Fortran95Lexer.END_FORALL for t in tokens)

    def test_f95_intrinsic_tokens(self):
        """
        Modern intrinsic function tokens used by the F90/F95 core
        should be present (CPU_TIME is new in Fortran 95; the others
        originate in Fortran 90 but remain part of the F95 baseline).
        """
        intrinsic_examples = {
            "CEILING": Fortran95Lexer.CEILING_INTRINSIC,
            "FLOOR": Fortran95Lexer.FLOOR_INTRINSIC,
            "MODULO": Fortran95Lexer.MODULO_INTRINSIC,
            "BIT_SIZE": Fortran95Lexer.BIT_SIZE_INTRINSIC,
            "BTEST": Fortran95Lexer.BTEST_INTRINSIC,
            "IAND": Fortran95Lexer.IAND_INTRINSIC,
            "IBITS": Fortran95Lexer.IBITS_INTRINSIC,
            "TRANSFER": Fortran95Lexer.TRANSFER_INTRINSIC,
            "CPU_TIME": Fortran95Lexer.CPU_TIME_INTRINSIC,
            "SYSTEM_CLOCK": Fortran95Lexer.SYSTEM_CLOCK_INTRINSIC,
        }

        for name, expected_type in intrinsic_examples.items():
            tokens = self.get_tokens(name)
            assert tokens, f"No tokens for {name}"
            assert tokens[0].type == expected_type


class TestFortran95Parser:
    """Test F95 parser rules for selected language constructs."""

    def create_parser_for_rule(self, text: str):
        lexer = Fortran95Lexer(InputStream(text))
        stream = CommonTokenStream(lexer)
        parser = Fortran95Parser(stream)
        return parser

    def test_pure_and_elemental_procedures(self):
        """PURE and ELEMENTAL procedure headers parse."""
        pure_fun = "PURE FUNCTION f(x) RESULT(y)"
        parser = self.create_parser_for_rule(pure_fun)
        tree = parser.pure_function_stmt()
        assert tree is not None
        assert parser.getNumberOfSyntaxErrors() == 0

        elemental_sub = "ELEMENTAL SUBROUTINE saxpy(a, x, y)"
        parser = self.create_parser_for_rule(elemental_sub)
        tree = parser.elemental_subroutine_stmt()
        assert tree is not None
        assert parser.getNumberOfSyntaxErrors() == 0

    def test_forall_construct_and_stmt(self):
        """FORALL construct and single-statement form parse."""
        stmt_code = "FORALL (i = 1:10) x(i) = i"
        parser = self.create_parser_for_rule(stmt_code)
        tree = parser.forall_stmt()
        assert tree is not None
        assert parser.getNumberOfSyntaxErrors() == 0

        construct_code = "FORALL (i = 1:10) x(i) = i END FORALL"
        parser = self.create_parser_for_rule(construct_code)
        tree = parser.forall_construct()
        assert tree is not None
        assert parser.getNumberOfSyntaxErrors() == 0

    def test_where_construct(self):
        """Enhanced WHERE / ELSEWHERE construct parses."""
        code = "WHERE (x > 0) x = 1 ELSEWHERE x = 2 END WHERE"
        parser = self.create_parser_for_rule(code)
        tree = parser.where_construct_f95()
        assert tree is not None
        assert parser.getNumberOfSyntaxErrors() == 0

    def test_array_constructor(self):
        """F95 array constructor using (/ ... /) parses."""
        code = "(/ 1, 2, 3 /)"
        parser = self.create_parser_for_rule(code)
        tree = parser.array_constructor_f95()
        assert tree is not None
        assert parser.getNumberOfSyntaxErrors() == 0
