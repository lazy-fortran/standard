! IBM 704 FORTRAN hardware-specific IF statements
! Demonstrates sense switches, sense lights, and overflow checks
        SENSE LIGHT 1
        IF (SENSE SWITCH 1) 10, 20
10      X = Y / Z
        IF DIVIDE CHECK 100, 30
100     ERR = 1
        GOTO 40
30      IF (SENSE LIGHT 1) 40, 50
40      A = B * C
        IF ACCUMULATOR OVERFLOW 60, 70
60      ERR = 2
        GOTO 80
70      IF QUOTIENT OVERFLOW 80, 90
80      CONTINUE
90      SENSE LIGHT 2
        IF (SENSE LIGHT 2) 95, 99
95      CONTINUE
99      CONTINUE
20      CONTINUE
50      CONTINUE
        END
