! Fortran 90: EXIT in nested DO loops with construct names
! ISO/IEC 1539:1991 Section 8.1.4.4.4
program test_nested_exit
    implicit none
    integer :: i, j

    outer: do i = 1, 10
        inner: do j = 1, 10
            if (j == 5) exit inner
            if (i == 5) exit outer
            print *, i, j
        end do inner
    end do outer
end program test_nested_exit
