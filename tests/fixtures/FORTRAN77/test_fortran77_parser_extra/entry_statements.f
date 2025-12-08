C     ENTRY statement examples per ISO 1539:1980 Section 15.7
C     ENTRY provides alternate entry points into subprograms
      SUBROUTINE CALC(A, B, RESULT)
      REAL A, B, RESULT
      RESULT = A + B
      RETURN
C
C     Alternate entry point with different arguments
      ENTRY MULT(X, Y, RESULT)
      RESULT = X * Y
      RETURN
C
C     Entry point with no arguments
      ENTRY RESET
      RESULT = 0.0
      RETURN
C
C     Entry point with alternate return specifier
      ENTRY SAFEDIV(A, B, RESULT, *)
      IF (B .EQ. 0.0) RETURN 1
      RESULT = A / B
      RETURN
      END
C
C     Function with ENTRY statements
      REAL FUNCTION COMPUTE(X)
      REAL X
      COMPUTE = X * X
      RETURN
C
C     Alternate entry for cube calculation
      ENTRY CUBE(X)
      CUBE = X * X * X
      RETURN
C
C     Entry with empty argument list
      ENTRY GETPI()
      GETPI = 3.14159
      RETURN
      END
