        N = 5
        FACT = 1
        I = 1
        10 IF (I .GT. N) 20, 15, 20
        15 FACT = FACT * I
        I = I + 1
        GOTO 10
        20 WRITE FACT
        STOP

