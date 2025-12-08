      EXTERNAL MYFUNC, MYSUB
      INTRINSIC SIN, COS, SQRT, ABS
      REAL X, Y, Z
      X = 1.0
      Y = SIN(X)
      Z = MYFUNC(X, Y)
      CALL MYSUB(Z)
      PRINT 100, Z
100   FORMAT (F10.5)
      END
