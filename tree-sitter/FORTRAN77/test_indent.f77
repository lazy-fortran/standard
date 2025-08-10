      CHARACTER*20 NAME
      INTEGER I
      LOGICAL FLAG
      
      NAME = 'HELLO WORLD'
      I = 10
      FLAG = .TRUE.
      
      IF (FLAG .AND. I .GT. 5) THEN
          PRINT *, NAME
          I = I + 1
      ELSE
          PRINT *, 'NOT TRUE'
      ENDIF
      
      DO WHILE (I .LE. 15)
          I = I + 1
      ENDDO
      
      END