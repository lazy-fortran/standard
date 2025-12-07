        N = 10
        DO 100 I = 1, N
        DO 200 J = 1, N
        IF (A(I) .GT. A(J)) 10, 200, 10
        10 TEMP = A(I)
        A(I) = A(J)
        A(J) = TEMP  
        200 CONTINUE
        100 CONTINUE

