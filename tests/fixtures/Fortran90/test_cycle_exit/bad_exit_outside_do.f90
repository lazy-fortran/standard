! Fortran 90: Invalid - EXIT outside any DO construct
! ISO/IEC 1539:1991 Section 8.1.4.4.4
! ERROR: EXIT statement outside any DO construct
program bad_exit_outside_do
    implicit none
    exit
end program bad_exit_outside_do
