
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

      SUBROUTINE FILL_MATRIX(MAT) BIND(C, NAME="FILL_MATRIX")
        TYPE(MATRIX_T(8,3,3)), INTENT(OUT) :: MAT
        INTEGER :: I, J
        DO I = 1, 3
          DO J = 1, 3
            MAT%DATA(I,J) = REAL(I*J,8)
          END DO
        END DO
      END SUBROUTINE FILL_MATRIX

      END MODULE F2003_FIXED_PDT_C
