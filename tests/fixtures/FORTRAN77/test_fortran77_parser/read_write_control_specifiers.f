      PROGRAM IOCONTROL
      INTEGER IOS, U, REC_NUM, IOERR
      REAL X(100), REC_DATA
      CHARACTER*20 REC_BUFFER

!     Direct-access READ with REC specifier
      U = 10
      READ (U, REC=5) X
      READ (U, REC=REC_NUM) X
      READ (U, '(F10.2)', REC=10) REC_DATA

!     Direct-access WRITE with REC specifier
      WRITE (U, REC=5) X
      WRITE (U, REC=REC_NUM) X
      WRITE (U, '(A)', REC=20) REC_BUFFER

!     Sequential READ with IOSTAT specifier
      READ (U, *) X
      READ (U, IOSTAT=IOS) X
      READ (U, *, IOSTAT=IOS) X
      READ (U, '(F10.2)', IOSTAT=IOS) REC_DATA

!     Sequential WRITE with IOSTAT specifier
      WRITE (U, *) X
      WRITE (U, IOSTAT=IOS) X
      WRITE (U, '(A)', IOSTAT=IOS) REC_BUFFER

!     READ with ERR specifier for error handling
      READ (U, *, ERR=100) X
      READ (U, '(F10.2)', ERR=100) REC_DATA
      READ (U, IOSTAT=IOS, ERR=100) X

!     Combined specifiers REC + IOSTAT + ERR
      READ (U, REC=5, IOSTAT=IOS, ERR=100) X
      WRITE (U, REC=5, IOSTAT=IOS) X

  100 CONTINUE
      END
