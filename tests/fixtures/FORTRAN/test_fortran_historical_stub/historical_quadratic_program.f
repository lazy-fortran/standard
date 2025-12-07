        READ 100, A, B, C
        DISC = B*B - 4.0*A*C
        IF (DISC) 10, 20, 30
10      PRINT 200
        GOTO 40
20      ROOT = -B/(2.0*A)
        PRINT 300, ROOT
        GOTO 40
30      ROOT1 = (-B + SQRT(DISC))/(2.0*A)
        ROOT2 = (-B - SQRT(DISC))/(2.0*A)
        PRINT 400, ROOT1, ROOT2
40      STOP
100     FORMAT (3F10.2)
200     FORMAT (12HNO REAL ROOTS)
300     FORMAT (11HSINGLE ROOT, F10.4)
400     FORMAT (10HTWO ROOTS:, 2F10.4)
        END

