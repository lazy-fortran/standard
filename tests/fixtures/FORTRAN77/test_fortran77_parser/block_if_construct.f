      PROGRAM TESTBLKIF
      INTEGER X, Y
      REAL R

      X = 5
      Y = 0
      R = 0.0

      IF (X .GT. 0) THEN
          Y = 1
      END IF

      IF (X .GT. 10) THEN
          Y = 2
      ELSE
          Y = 3
      END IF

      IF (X .LT. 0) THEN
          PRINT *, 'NEGATIVE'
      ELSE IF (X .EQ. 0) THEN
          PRINT *, 'ZERO'
      ELSE IF (X .GT. 10) THEN
          PRINT *, 'LARGE'
      ELSE
          PRINT *, 'SMALL POSITIVE'
      END IF

      IF (X .GT. 0) THEN
          IF (X .GT. 10) THEN
              Y = 100
          ELSE
              Y = 50
          END IF
      END IF

      IF (X .LT. 0) THEN
          IF (R .LT. 0.0) THEN
              PRINT *, 'BOTH NEGATIVE'
          ELSE
              PRINT *, 'X NEG, R POS'
          END IF
      ELSE IF (X .EQ. 0) THEN
          PRINT *, 'X IS ZERO'
      ELSE
          IF (R .GT. 0.0) THEN
              PRINT *, 'BOTH POSITIVE'
          ELSE
              PRINT *, 'X POS, R NOT POSITIVE'
          END IF
      END IF

      STOP
      END
