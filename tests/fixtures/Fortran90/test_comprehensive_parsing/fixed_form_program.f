!     This is a fixed-form Fortran 90 program
!     Using F90 constructs: free-form comments and DO...END DO
      PROGRAM TESTPROG
      INTEGER I, N
      PARAMETER (N=10)
      REAL A(N)

!     Initialize array using F90 DO...END DO
      DO I=1,N
          A(I) = I * 2
      END DO

!     Print results using F90 list-directed PRINT
      DO I=1,N
          PRINT *, A(I)
      END DO

      END

