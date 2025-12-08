C     FORTRAN 1957 ARITHMETIC EXAMPLE                                 00000010
C     IBM 704 AUTHENTIC CARD LAYOUT PER C28-6003                       00000020
      READ 100, A, B                                                    00000030
C     COMPUTE SUM AND PRODUCT                                          00000040
      SUM = A + B                                                       00000050
      PROD = A * B                                                      00000060
      PRINT 200, SUM, PROD                                              00000070
      STOP                                                              00000080
  100 FORMAT (2F10.2)                                                   00000090
  200 FORMAT (2F15.4)                                                   00000100
      END                                                               00000110
