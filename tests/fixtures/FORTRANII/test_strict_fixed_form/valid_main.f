C     FORTRAN II MAIN PROGRAM - STRICT 80-COLUMN CARD FORMAT           00000010
C     Reference: IBM C28-6000-2 (1958)                                 00000020
C     DEMONSTRATES: COMMON, ARITHMETIC IF, GOTO, FORMAT                00000030
      COMMON A, B, RESULT                                               00000040
      A = 1.0                                                           00000050
      B = 2.0                                                           00000060
C     COMPUTE SUM OF SQUARES                                           00000080
      RESULT = A * A + B * B                                            00000090
C     TEST ARITHMETIC IF                                               00000100
      IF (RESULT - 5.0) 10, 20, 30                                      00000110
   10 PRINT 100, RESULT                                                 00000120
      GOTO 40                                                           00000130
   20 PRINT 100, RESULT                                                 00000140
      GOTO 40                                                           00000150
   30 PRINT 100, RESULT                                                 00000160
   40 CONTINUE                                                          00000170
  100 FORMAT (6HRESULT, F10.4)                                          00000180
      STOP                                                              00000190
      END                                                               00000200
