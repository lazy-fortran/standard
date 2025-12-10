! Test REWIND statement in Fortran 95 program
! ISO/IEC 1539-1:1997 Section 9.4.3 (R925)
program test_rewind_f95
    implicit none

    rewind(unit=10)

end program test_rewind_f95
