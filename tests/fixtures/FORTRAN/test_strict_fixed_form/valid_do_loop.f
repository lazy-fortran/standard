C     FORTRAN 1957 DO LOOP EXAMPLE                                     00000010
C     IBM 704 AUTHENTIC CARD LAYOUT PER C28-6003                       00000020
      DIMENSION A(10)                                                   00000030
C     INITIALIZE ARRAY                                                 00000040
      DO 10 I = 1, 10                                                   00000050
      A(I) = I * 2.0                                                    00000060
   10 CONTINUE                                                          00000070
C     COMPUTE SUM                                                      00000080
      SUM = 0.0                                                         00000090
      DO 20 J = 1, 10                                                   00000100
      SUM = SUM + A(J)                                                  00000110
   20 CONTINUE                                                          00000120
      PRINT 100, SUM                                                    00000130
      STOP                                                              00000140
  100 FORMAT (F15.4)                                                    00000150
      END                                                               00000160
