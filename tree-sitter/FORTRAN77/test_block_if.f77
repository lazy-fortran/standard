      INTEGER X, Y, Z
      X = 10
      Y = 20
      Z = 30
      
      IF (X .GT. Y) THEN
          PRINT *, 'X IS GREATER'
          Z = X
      ELSEIF (Y .GT. Z) THEN
          PRINT *, 'Y IS GREATER'
          Z = Y
      ELSEIF (X .EQ. Y) THEN
          PRINT *, 'X EQUALS Y'
          Z = 0
      ELSE
          PRINT *, 'Z IS GREATEST'
      ENDIF