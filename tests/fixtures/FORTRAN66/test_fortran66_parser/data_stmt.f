      INTEGER I, J, K
      REAL X, Y, Z
      DIMENSION ARR(10), MAT(5,5)
      LOGICAL FLAG
      DATA X, Y, Z /1.0, 2.0, 3.0/
      DATA I, J, K /1, 2, 3/
      DATA FLAG /.TRUE./
      DATA ARR /10*0.0/
      DATA MAT /25*1.0/
      PRINT 100, X, Y, Z
100   FORMAT (3F10.2)
      END
