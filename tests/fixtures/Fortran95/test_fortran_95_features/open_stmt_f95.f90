! Test OPEN statement in Fortran 95 program
! ISO/IEC 1539-1:1997 Section 9.3.1 (R904)
program test_open_f95
    implicit none

    open(unit=10, file='data.txt', status='new')

end program test_open_f95
