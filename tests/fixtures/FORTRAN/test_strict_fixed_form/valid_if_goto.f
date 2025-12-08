C     FORTRAN 1957 IF AND GO TO EXAMPLE                                00000010
C     IBM 704 AUTHENTIC CARD LAYOUT PER C28-6003                       00000020
      READ 100, X                                                       00000030
C     ARITHMETIC IF - THREE WAY BRANCH                                 00000040
      IF (X) 10, 20, 30                                                 00000050
C     X IS NEGATIVE                                                    00000060
   10 PRINT 200                                                         00000070
      GO TO 40                                                          00000080
C     X IS ZERO                                                        00000090
   20 PRINT 300                                                         00000100
      GO TO 40                                                          00000110
C     X IS POSITIVE                                                    00000120
   30 PRINT 400                                                         00000130
   40 STOP                                                              00000140
  100 FORMAT (F10.2)                                                    00000150
  200 FORMAT (12HVALUE IS NEG)                                          00000160
  300 FORMAT (13HVALUE IS ZERO)                                         00000170
  400 FORMAT (12HVALUE IS POS)                                          00000180
      END                                                               00000190
