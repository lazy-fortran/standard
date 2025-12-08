      SUBROUTINE SHARE(A, B, C)
      COMMON /DATA/ X, Y, Z
      COMMON /WORK/ ARRAY(100)
      C = A + B + X
      RETURN
      END
