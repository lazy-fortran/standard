        DO 100 I = 1, 3
        DO 200 J = 1, 3
        IF (I .EQ. J) 10, 20, 20
        10 MATRIX(I, J) = 1.0
        GOTO 200
        20 MATRIX(I, J) = 0.0
        200 CONTINUE
        100 CONTINUE

