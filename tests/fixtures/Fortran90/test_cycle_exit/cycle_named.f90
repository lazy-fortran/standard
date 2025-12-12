! Fortran 90: CYCLE with construct name in named DO loop
! ISO/IEC 1539:1991 Section 8.1.4.4.3
program test_cycle_named
    implicit none
    integer :: i

    outer: do i = 1, 10
        if (i == 5) cycle outer
        print *, i
    end do outer
end program test_cycle_named
