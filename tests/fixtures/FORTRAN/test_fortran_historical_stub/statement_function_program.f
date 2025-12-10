      POLYF(X) = C0 + X*(C1 + X*C2)
      MULTF(I,J) = I*J
      C0 = 1.0
      C1 = 2.0
      C2 = 3.0
      Y = POLYF(2.0)
      Z = MULTF(3,4)
      PRINT 10, Y, Z
10    FORMAT(2F10.2)
      END
