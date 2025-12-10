! Test ENDFILE statement in Fortran 95 program
! ISO/IEC 1539-1:1997 Section 9.4.2 (R924)
program test_endfile_f95
    implicit none

    endfile(unit=10)

end program test_endfile_f95
