        DIMENSION A(100), B(10,20), C(5,5,5)
        A(I) = B(I,J) + C(I,J,K)
        PRINT 100, A(25)
100     FORMAT (F15.6)

