! Test CLOSE statement in Fortran 95 program
! ISO/IEC 1539-1:1997 Section 9.3.2 (R908)
program test_close_f95
    implicit none

    close(unit=10, status='keep')

end program test_close_f95
