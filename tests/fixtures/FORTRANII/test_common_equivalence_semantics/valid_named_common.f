      SUBROUTINE MAIN
      INTEGER A, B
      REAL X, Y
      COMMON /DATA/ A, B, X, Y
      A = 1
      CALL SUB1
      RETURN
      END

      SUBROUTINE SUB1
      INTEGER A, B
      REAL X, Y
      COMMON /DATA/ A, B, X, Y
      WRITE A
      RETURN
      END
