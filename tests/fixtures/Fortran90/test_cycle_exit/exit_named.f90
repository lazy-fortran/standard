! Fortran 90: EXIT with construct name in named DO loop
! ISO/IEC 1539:1991 Section 8.1.4.4.4
program test_exit_named
    implicit none
    integer :: i

    outer: do i = 1, 10
        if (i == 5) exit outer
        print *, i
    end do outer
end program test_exit_named
