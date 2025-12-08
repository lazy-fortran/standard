C     FORTRAN II SUBROUTINE - STRICT 80-COLUMN CARD FORMAT             00000010
C     Reference: IBM C28-6000-2 (1958) Part I, Chapter 2               00000020
C     Columns: 1-5=label, 6=continuation, 7-72=statement, 73-80=seq    00000030
      SUBROUTINE SWAP(X, Y)                                             00000040
C     SWAP TWO REAL VALUES USING A TEMPORARY VARIABLE                  00000050
C     Note: FORTRAN II uses implicit typing (I-N integer, else real)   00000060
      TEMP = X                                                          00000070
      X = Y                                                             00000080
      Y = TEMP                                                          00000090
      RETURN                                                            00000100
      END                                                               00000110
