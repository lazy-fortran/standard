! Fortran 90: Basic CYCLE statement in DO loop
! ISO/IEC 1539:1991 Section 8.1.4.4.3
program test_cycle_basic
    implicit none
    integer :: i

    do i = 1, 10
        if (i == 5) cycle
        print *, i
    end do
end program test_cycle_basic
