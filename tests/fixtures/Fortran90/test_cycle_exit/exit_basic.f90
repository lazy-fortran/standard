! Fortran 90: Basic EXIT statement in DO loop
! ISO/IEC 1539:1991 Section 8.1.4.4.4
program test_exit_basic
    implicit none
    integer :: i

    do i = 1, 10
        if (i == 5) exit
        print *, i
    end do
end program test_exit_basic
