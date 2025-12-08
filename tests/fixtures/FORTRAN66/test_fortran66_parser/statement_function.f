      REAL SQUARE, CUBE, AREA, DIST
      SQUARE(X) = X * X
      AREA(L, W) = L * W
      CUBE(X) = SQUARE(X) * X
      DIST(X1, Y1, X2, Y2) = SQRT((X2-X1)**2 + (Y2-Y1)**2)
      REAL R, L, W
      R = 5.0
      L = 10.0
      W = 4.0
      PRINT 100, SQUARE(R)
      PRINT 100, CUBE(R)
      PRINT 100, AREA(L, W)
      PRINT 100, DIST(0.0, 0.0, 3.0, 4.0)
  100 FORMAT(F10.2)
      END
