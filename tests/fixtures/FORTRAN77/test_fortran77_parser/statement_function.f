      PROGRAM STMTFUN
      REAL SQUARE, CUBE, AREA, DIST
      REAL R, L, W, X1, Y1, X2, Y2
      REAL PI
      SQUARE(X) = X * X
      AREA(L, W) = L * W
      CUBE(X) = SQUARE(X) * X
      DIST(X1, Y1, X2, Y2) = SQRT((X2-X1)**2 + (Y2-Y1)**2)
      R = 5.0
      L = 10.0
      W = 4.0
      IF (SQUARE(R) .GT. 20.0) THEN
          PRINT *, SQUARE(R)
      END IF
      X1 = 0.0
      Y1 = 0.0
      X2 = 3.0
      Y2 = 4.0
      PRINT *, DIST(X1, Y1, X2, Y2)
      PRINT *, CUBE(R)
      PRINT *, AREA(L, W)
      END
