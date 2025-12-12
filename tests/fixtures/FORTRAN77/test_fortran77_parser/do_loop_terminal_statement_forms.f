      PROGRAM DO_LOOP_TERMINALS
      REAL A(5), B(5), C(5), TEMP
      DO 10 I = 1, 5
      A(I) = B(I) + C(I)
 10   A(I) = A(I) * 2.0
      DO 20 J = 1, 3
      B(J) = J
 20   CALL HANDLE(B(J))
      DO 30 K = 1, 2
      C(K) = K * 2
 30   READ(5,*) C(K)
      DO 31 L = 1, 2
      C(L) = L - 1
 31   WRITE(6,*) C(L)
      DO 40 M = 1, 4
      TEMP = M - 2
 40   IF (TEMP) 50, 60, 70
 50   CONTINUE
 60   CONTINUE
 70   CONTINUE
      END
