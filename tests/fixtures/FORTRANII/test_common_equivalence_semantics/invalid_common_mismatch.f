      PROGRAM TEST
      INTEGER A, B
      COMMON /BLOCK1/ A, B
      CALL SUB1
      STOP
      END

      SUBROUTINE SUB1
      INTEGER A, B, C
      COMMON /BLOCK1/ A, B, C
      RETURN
      END
