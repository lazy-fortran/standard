      PROGRAM TEST_DATA_IMPLIED_DO_NESTED
      INTEGER B(5,5)
      REAL C(3,4)
      DATA ((B(I,J), J=1,5), I=1,5) / 25*0 /
      DATA ((C(I,J), J=1,4), I=1,3) / 12*0.0 /
      PRINT *, 'Nested implied-DO DATA test'
      STOP
      END
