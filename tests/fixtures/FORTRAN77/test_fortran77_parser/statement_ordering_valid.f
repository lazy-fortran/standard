      PROGRAM ORDEROK
      IMPLICIT REAL (A-H, O-Z)
      INTEGER I
      REAL X
      DATA X /1.0/
      F(Y) = Y + X
      I = 1
      X = F(2.0)
  10  FORMAT(1X, F5.1)
      PRINT 10, X
      END
