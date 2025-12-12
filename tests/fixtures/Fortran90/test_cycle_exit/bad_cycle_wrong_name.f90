! Fortran 90: Invalid - CYCLE with nonexistent construct name
! ISO/IEC 1539:1991 Section 8.1.4.4.3
! ERROR: CYCLE construct name does not match active DO construct
program bad_cycle_wrong_name
    implicit none
    integer :: i

    outer: do i = 1, 10
        if (i == 5) cycle nonexistent
    end do outer
end program bad_cycle_wrong_name
