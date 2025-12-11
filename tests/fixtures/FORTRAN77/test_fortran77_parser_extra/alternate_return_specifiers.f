      PROGRAM ALTRET
      REAL X, Y
      CALL SOLVE(X, Y, *100, *200)
      STOP
 100  WRITE(*,*) 'ALT RETURN 100'
      STOP
 200  WRITE(*,*) 'ALT RETURN 200'
      STOP

      SUBROUTINE SOLVE(A, B)
      REAL A, B
      IF (A .GT. B) RETURN 2
      RETURN 1
      END
