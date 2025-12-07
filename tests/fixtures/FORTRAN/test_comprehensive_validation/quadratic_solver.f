        A = 1.0
        B = -5.0
        C = 6.0
        DISC = B ** 2 - 4.0 * A * C
        IF (DISC) 10, 20, 30
        10 WRITE -1
        GOTO 40
        20 X = -B / (2.0 * A)
        WRITE X
        GOTO 40
        30 SQRT_D = DISC ** 0.5  
        X1 = (-B + SQRT_D) / (2.0 * A)
        X2 = (-B - SQRT_D) / (2.0 * A)
        WRITE X1
        WRITE X2
        40 STOP

