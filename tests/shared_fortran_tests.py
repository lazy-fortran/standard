#!/usr/bin/env python3
"""Shared test case data and helpers for FORTRAN 66 and FORTRAN 77 parser tests.

This module provides reusable test case lists and a mixin class for testing
statement functions vs array element assignments. Both FORTRAN 66 (X3.9-1966)
and FORTRAN 77 (X3.9-1978) share identical grammar rules for these constructs.
"""

from typing import List, Tuple

STATEMENT_FUNCTION_SIMPLE_CASES: List[str] = [
    "F(X) = X * X",
    "AREA(R) = 3.14159 * R * R",
    "SQUARE(N) = N ** 2",
    "DOUBLE(X) = 2.0 * X",
]

STATEMENT_FUNCTION_MULTI_ARG_CASES: List[str] = [
    "SUM(A, B) = A + B",
    "AREA(L, W) = L * W",
    "AVG(X, Y, Z) = (X + Y + Z) / 3.0",
    "DIST(X1, Y1, X2, Y2) = SQRT((X2-X1)**2 + (Y2-Y1)**2)",
]

STATEMENT_FUNCTION_IN_BODY_CASES: List[str] = [
    "F(X) = X * X",
    "AREA(L, W) = L * W",
]

ARRAY_ELEMENT_LITERAL_INDEX_CASES: List[str] = [
    "A(1) = 2.0",
    "B(100) = X",
    "MAT(1, 2) = 3.0",
    "ARR(1, 2, 3) = Y",
]

ARRAY_ELEMENT_EXPR_INDEX_CASES: List[str] = [
    "A(I+1) = X",
    "B(2*J) = Y",
    "C(I-1, J+1) = Z",
    "D(N/2) = W",
]

ARRAY_ELEMENT_ASSIGNMENT_CASES: List[str] = [
    "A(1) = 2.0",
    "B(I+1) = X * Y",
    "MAT(1, 2) = 3.0",
    "C(I-1, J+1) = Z",
    "ARR(N/2) = W + 1.0",
]

STATEMENT_BODY_DISAMBIGUATION_CASES: List[Tuple[str, str]] = [
    ("F(X) = X * X", "Statement_function_stmt"),
    ("A(1) = 2.0", "Assignment_stmt"),
    ("B(I+1) = X", "Assignment_stmt"),
    ("SUM(A, B) = A + B", "Statement_function_stmt"),
    ("MAT(1, 2) = 3.0", "Assignment_stmt"),
]


class StatementFunctionTestMixin:
    """Mixin providing shared statement function vs array element tests.

    Subclasses must provide:
    - self.LexerClass: The ANTLR lexer class (e.g., FORTRAN66Lexer)
    - self.ParserClass: The ANTLR parser class (e.g., FORTRAN66Parser)
    - self.parse(text, rule_name): Method to parse text with a given rule
    - self.fixture_standard: Standard name for fixture loading (e.g., FORTRAN66)

    Both FORTRAN 66 (X3.9-1966 Section 7.2) and FORTRAN 77 (X3.9-1978 Section 8)
    define identical semantics for statement functions.
    """

    def test_statement_function_simple(self):
        """Test simple statement function definition."""
        for text in STATEMENT_FUNCTION_SIMPLE_CASES:
            with self.subTest(stmt_func=text):
                tree = self.parse(text, 'statement_function_stmt')
                self.assertIsNotNone(tree)

    def test_statement_function_multiple_args(self):
        """Test statement function with multiple arguments."""
        for text in STATEMENT_FUNCTION_MULTI_ARG_CASES:
            with self.subTest(stmt_func=text):
                tree = self.parse(text, 'statement_function_stmt')
                self.assertIsNotNone(tree)

    def test_statement_function_in_statement_body(self):
        """Test statement function as statement_body alternative."""
        for text in STATEMENT_FUNCTION_IN_BODY_CASES:
            with self.subTest(stmt=text):
                tree = self.parse(text, 'statement_body')
                self.assertIsNotNone(tree)

    def test_statement_function_fixture(self):
        """Test program with statement function definitions."""
        from fixture_utils import load_fixture
        program = load_fixture(
            self.fixture_standard,
            f"test_{self.fixture_standard.lower()}_parser",
            "statement_function.f",
        )
        tree = self.parse(program, 'main_program')
        self.assertIsNotNone(tree)

    def test_array_element_not_statement_function_literal_index(self):
        """Verify array element with literal index is NOT a statement function."""
        from antlr4 import InputStream, CommonTokenStream
        for text in ARRAY_ELEMENT_LITERAL_INDEX_CASES:
            with self.subTest(array_assignment=text):
                input_stream = InputStream(text)
                lexer = self.LexerClass(input_stream)
                token_stream = CommonTokenStream(lexer)
                parser = self.ParserClass(token_stream)
                parser.statement_function_stmt()
                self.assertGreater(
                    parser.getNumberOfSyntaxErrors(), 0,
                    f"'{text}' should NOT parse as statement_function_stmt"
                )

    def test_array_element_not_statement_function_expr_index(self):
        """Verify array element with expression index is NOT a statement function."""
        from antlr4 import InputStream, CommonTokenStream
        for text in ARRAY_ELEMENT_EXPR_INDEX_CASES:
            with self.subTest(array_assignment=text):
                input_stream = InputStream(text)
                lexer = self.LexerClass(input_stream)
                token_stream = CommonTokenStream(lexer)
                parser = self.ParserClass(token_stream)
                parser.statement_function_stmt()
                self.assertGreater(
                    parser.getNumberOfSyntaxErrors(), 0,
                    f"'{text}' should NOT parse as statement_function_stmt"
                )

    def test_array_element_parses_as_assignment_stmt(self):
        """Verify array element assignments parse correctly as assignment_stmt."""
        from antlr4 import InputStream, CommonTokenStream
        for text in ARRAY_ELEMENT_ASSIGNMENT_CASES:
            with self.subTest(array_assignment=text):
                input_stream = InputStream(text)
                lexer = self.LexerClass(input_stream)
                token_stream = CommonTokenStream(lexer)
                parser = self.ParserClass(token_stream)
                tree = parser.assignment_stmt()
                self.assertIsNotNone(tree)
                self.assertEqual(
                    parser.getNumberOfSyntaxErrors(), 0,
                    f"'{text}' should parse as assignment_stmt without errors"
                )

    def test_statement_body_disambiguates_array_vs_function(self):
        """Verify statement_body correctly parses array elements as assignments."""
        from antlr4 import InputStream, CommonTokenStream
        for text, expected_type in STATEMENT_BODY_DISAMBIGUATION_CASES:
            with self.subTest(stmt=text, expected=expected_type):
                input_stream = InputStream(text)
                lexer = self.LexerClass(input_stream)
                token_stream = CommonTokenStream(lexer)
                parser = self.ParserClass(token_stream)
                tree = parser.statement_body()
                self.assertIsNotNone(tree)
                child = tree.getChild(0)
                child_type = type(child).__name__
                self.assertIn(
                    expected_type, child_type,
                    f"'{text}' should parse as {expected_type}, got {child_type}"
                )
