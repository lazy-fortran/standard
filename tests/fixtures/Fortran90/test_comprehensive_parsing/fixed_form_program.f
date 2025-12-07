C     This is a fixed-form Fortran program
      PROGRAM TESTPROG
      INTEGER I, N
      PARAMETER (N=10)
      DIMENSION A(N)

C     Initialize array
      DO 100 I=1,N
          A(I) = I * 2
  100 CONTINUE

C     Print results
      DO 200 I=1,N
          WRITE(*,*) 'A(', I, ') = ', A(I)
  200 CONTINUE

      END

