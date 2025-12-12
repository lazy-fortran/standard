C     Valid Fortran 90 with continuation lines
      PROGRAM CONT_EXAMPLE
      IMPLICIT NONE
      REAL :: A, B, C
      A = 1.0
      B = 2.0
      C = A + B +
     &    1.0
      PRINT *, C
      END PROGRAM CONT_EXAMPLE
