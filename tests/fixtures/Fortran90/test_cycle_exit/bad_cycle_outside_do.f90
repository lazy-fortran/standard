! Fortran 90: Invalid - CYCLE outside any DO construct
! ISO/IEC 1539:1991 Section 8.1.4.4.3
! ERROR: CYCLE statement outside any DO construct
program bad_cycle_outside_do
    implicit none
    cycle
end program bad_cycle_outside_do
