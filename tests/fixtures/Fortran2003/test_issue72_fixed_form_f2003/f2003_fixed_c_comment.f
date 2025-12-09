
C*** Fixed-form comment without whitespace after C (issue #343)
C--- Another comment line with dash after C
c*** lowercase c comment with asterisks
C------------------------------------------------------
      PROGRAM F2003_FIXED_C_COMMENT
      IMPLICIT NONE
C This is a normal C comment with space
c This is a lowercase c comment with space
*     This is a star comment
      WRITE(*,*) 'FIXED-FORM C COMMENTS TEST'
C*** End of test program
      END PROGRAM F2003_FIXED_C_COMMENT
