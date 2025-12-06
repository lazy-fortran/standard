#!/usr/bin/env python3
"""
Issue #72 – Fortran 2003 fixed-form source with F2003 features

These tests exercise F2003 features written in traditional fixed-form
layout (upper-case, column-based style), to validate that the unified
lexer/parser handle both formats.
"""

import sys
from pathlib import Path

import pytest
from antlr4 import InputStream, CommonTokenStream

sys.path.append(str(Path(__file__).parent.parent.parent / "grammars"))

from Fortran2003Lexer import Fortran2003Lexer
from Fortran2003Parser import Fortran2003Parser


def parse_f2003(code: str):
    """Parse Fortran 2003 code and return (tree, errors, parser)."""
    input_stream = InputStream(code)
    lexer = Fortran2003Lexer(input_stream)
    parser = Fortran2003Parser(CommonTokenStream(lexer))
    tree = parser.program_unit_f2003()
    return tree, parser.getNumberOfSyntaxErrors(), parser


class TestF2003FixedFormFeatures:
    """Fixed-form tests for representative F2003 features."""

    def test_fixed_form_oop_and_class_star(self):
        """Fixed-form module with type-bound procedure and CLASS(*) dummy."""
        code = """
C   F2003 OOP and CLASS(*) in fixed-form
      MODULE F2003_FIXED_OOP
      IMPLICIT NONE

      TYPE SHAPE_T
        REAL :: AREA
      CONTAINS
        PROCEDURE :: PRINT_AREA
      END TYPE SHAPE_T

      CONTAINS

      SUBROUTINE PRINT_AREA(THIS)
        CLASS(SHAPE_T), INTENT(IN) :: THIS
        WRITE(*,*) 'AREA =', THIS%AREA
      END SUBROUTINE PRINT_AREA

      END MODULE F2003_FIXED_OOP
"""
        tree, errors, _ = parse_f2003(code)
        assert tree is not None
        assert errors == 0

    def test_fixed_form_select_type_and_associate(self):
        """SELECT TYPE and ASSOCIATE in fixed-form style."""
        code = """
C   F2003 SELECT TYPE and ASSOCIATE in fixed form
      MODULE F2003_FIXED_SELECT
      IMPLICIT NONE

      TYPE SHAPE_T
        REAL :: R
      END TYPE SHAPE_T

      CONTAINS

      SUBROUTINE RENDER(OBJ)
        CLASS(*), INTENT(IN) :: OBJ
        SELECT TYPE (OBJ)
        TYPE IS (SHAPE_T)
          ASSOCIATE (RADIUS => OBJ%R)
            PRINT *, 'RADIUS =', RADIUS
          END ASSOCIATE
        CLASS DEFAULT
          PRINT *, 'UNKNOWN SHAPE'
        END SELECT
      END SUBROUTINE RENDER

      END MODULE F2003_FIXED_SELECT
"""
        tree, errors, _ = parse_f2003(code)
        assert tree is not None
        assert errors == 0

    def test_fixed_form_pdt_and_bind_c(self):
        """PDT and BIND(C) usage in fixed-form."""
        code = """
C   F2003 PDT and BIND(C) in fixed form
      MODULE F2003_FIXED_PDT_C
      USE ISO_C_BINDING
      IMPLICIT NONE

      TYPE MATRIX_T(K,M,N)
        INTEGER, KIND :: K
        INTEGER, LEN  :: M, N
        REAL(K)       :: DATA(M,N)
      END TYPE MATRIX_T

      TYPE, BIND(C) :: POINT_T
        REAL(C_DOUBLE) :: X
        REAL(C_DOUBLE) :: Y
      END TYPE POINT_T

      CONTAINS

      SUBROUTINE FILL_MATRIX(MAT) BIND(C, NAME=\"FILL_MATRIX\")
        TYPE(MATRIX_T(8,3,3)), INTENT(OUT) :: MAT
        INTEGER :: I, J
        DO I = 1, 3
          DO J = 1, 3
            MAT%DATA(I,J) = REAL(I*J,8)
          END DO
        END DO
      END SUBROUTINE FILL_MATRIX

      END MODULE F2003_FIXED_PDT_C
"""
        tree, errors, _ = parse_f2003(code)
        assert tree is not None
        assert errors == 0
