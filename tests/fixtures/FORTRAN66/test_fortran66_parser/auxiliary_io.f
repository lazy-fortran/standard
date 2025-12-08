      INTEGER I
      DIMENSION ARR(100)
      REWIND 5
      BACKSPACE 5
      ENDFILE 5
      DO 10 I = 1, 10
         ARR(I) = I * 2
   10 CONTINUE
      REWIND 6
      ENDFILE 6
      END
