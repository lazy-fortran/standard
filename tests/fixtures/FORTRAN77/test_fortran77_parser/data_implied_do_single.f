      PROGRAM TEST_DATA_IMPLIED_DO_SINGLE
      REAL A(10)
      INTEGER B(5)
      LOGICAL FLAG(3)
      DATA (A(I), I=1,10) / 10*0.0 /
      DATA (B(I), I=1,5) / 5*1 /
      DATA (FLAG(I), I=1,3) / 3*.TRUE. /
      PRINT *, 'Single-level implied-DO DATA test'
      STOP
      END
