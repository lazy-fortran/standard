C     FORTRAN II FUNCTION - STRICT 80-COLUMN CARD FORMAT               00000010
C     Reference: IBM C28-6000-2 (1958) Part I, Chapter 3, Sec 3.3      00000020
      FUNCTION MAX(A, B)                                                00000030
C     RETURN THE MAXIMUM OF TWO VALUES                                 00000040
      IF (A - B) 10, 20, 20                                             00000050
   10 MAX = B                                                           00000060
      RETURN                                                            00000070
   20 MAX = A                                                           00000080
      RETURN                                                            00000090
      END                                                               00000100
