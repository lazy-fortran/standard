PROGRAM CHARTEST
      CHARACTER*20 NAME, FIRST, LAST
      CHARACTER*80 MSG
      CHARACTER*1 CH

      NAME = 'JOHN DOE'
      CH = 'X'
      MSG = 'HELLO WORLD'

      MSG = 'IT''S A TEST'
      MSG = 'DON''T PANIC'
      MSG = 'A''B''C'

      FIRST = 'JOHN'
      LAST = 'DOE'

      PRINT *, 'ENTER YOUR NAME:'
      READ *, NAME
      PRINT *, 'HELLO'
      WRITE (6, *) 'RESULT'

      IF (NAME .EQ. 'JOHN DOE') THEN
          PRINT *, 'NAME MATCHES'
      ELSE IF (NAME .LT. 'M') THEN
          PRINT *, 'NAME STARTS WITH A-L'
      ELSE
          PRINT *, 'NAME STARTS WITH M-Z'
      END IF

      STOP
      END
