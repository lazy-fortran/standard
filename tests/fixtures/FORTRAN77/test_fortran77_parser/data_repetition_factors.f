      PROGRAM TEST_DATA_REPEAT
      INTEGER A(10), B(5)
      REAL X(100), Y(6)
      LOGICAL FLAG(4)

      DATA A /10*0/
      DATA B /3*1, 2*0/
      DATA X /100*0.0/

      DATA Y /1.0, 2*2.0, 3*3.0/

      DATA FLAG /4*.TRUE./

      PRINT *, 'Test completed'
      STOP
      END
