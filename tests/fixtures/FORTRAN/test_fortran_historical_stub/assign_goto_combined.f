        I = 1
        ASSIGN 100 TO JUMP
        IF (I) 10, 20, 30
10      ASSIGN 200 TO JUMP
        GOTO 40
20      ASSIGN 300 TO JUMP
        GOTO 40
30      ASSIGN 100 TO JUMP
40      GOTO JUMP, (100, 200, 300)
100     X = 1.0
        STOP
200     Y = 2.0
        STOP
300     Z = 3.0
        END
