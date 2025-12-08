C     FORTRAN II CONTINUATION CARD EXAMPLE                             00000010
C     Column 6 non-blank marks continuation (here using digits 1-9)    00000020
      SUBROUTINE LONGCALC(A, B, C, D, E, F,                             00000030
     1                    G, H, I, J, K, L)                             00000040
C     DEMONSTRATES CONTINUATION ACROSS MULTIPLE CARDS                  00000050
      RESULT = A + B + C + D + E + F +                                  00000060
     1         G + H + I + J + K + L                                    00000070
C     LONG EXPRESSION WITH MULTIPLE CONTINUATIONS                      00000080
      TOTAL = A * B * C *                                               00000090
     1        D * E * F *                                               00000100
     2        G * H * I *                                               00000110
     3        J * K * L                                                 00000120
      RETURN                                                            00000130
      END                                                               00000140
