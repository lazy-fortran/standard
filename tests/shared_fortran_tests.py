#!/usr/bin/env python3
"""Shared test case data and helpers for FORTRAN 66 and FORTRAN 77 parser tests.

This module provides reusable test case lists and helper methods for testing
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
