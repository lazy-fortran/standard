C     FORTRAN 1957 CONTINUATION EXAMPLE                                00000010
C     IBM 704 CARD LAYOUT WITH CONTINUATION IN COL 6                   00000020
      READ 100, A, B,                                                   00000030
     1         C, D,                                                    00000040
     2         E, F                                                     00000050
C     COMPLEX EXPRESSION WITH CONTINUATION                             00000060
      RESULT = A + B +                                                  00000070
     1         C * D -                                                  00000080
     2         E / F                                                    00000090
      PRINT 200, RESULT                                                 00000100
      STOP                                                              00000110
  100 FORMAT (6F10.2)                                                   00000120
  200 FORMAT (F20.8)                                                    00000130
      END                                                               00000140
