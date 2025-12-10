      INTEGER I, J
      REAL X, Y, Z
      DIMENSION ARR(10)
      DATA ARR /1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0/

  100 FORMAT (3F10.2)
  110 FORMAT (1H1)
  120 FORMAT (I5, E10.3)

      X = 3.14
      Y = 2.71
      Z = 1.41

      WRITE (6, 100) X, Y, Z
      WRITE (6, 110)
      DO 10 I = 1, 10
         ARR(I) = ARR(I) * 2.0
         WRITE (6, 120) I, ARR(I)
   10 CONTINUE
      END
