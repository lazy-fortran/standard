! Fortran 90: Invalid - EXIT with nonexistent construct name
! ISO/IEC 1539:1991 Section 8.1.4.4.4
! ERROR: EXIT construct name does not match active DO construct
program bad_exit_wrong_name
    implicit none
    integer :: i

    outer: do i = 1, 10
        if (i == 5) exit nonexistent
    end do outer
end program bad_exit_wrong_name
