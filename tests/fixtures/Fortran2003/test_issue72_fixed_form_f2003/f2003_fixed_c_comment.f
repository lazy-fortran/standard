
C*** Fixed-form comment without whitespace after C (issue #343)
CAnother comment line without space (ISO 1539-1:2004 Section 3.3.2)
c lowercase c comment without space
C------------------------------------------------------
      PROGRAM F2003_FIXED_C_COMMENT
      IMPLICIT NONE
C This is a normal C comment with space
c This is a lowercase c comment with space
*     This is a star comment
      WRITE(*,*) 'FIXED-FORM C COMMENTS TEST'
CEnd of test program
      END PROGRAM F2003_FIXED_C_COMMENT
