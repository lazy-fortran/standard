! Fortran 90: CYCLE in DO WHILE construct
! ISO/IEC 1539:1991 Section 8.1.4.4.3
program test_do_while_cycle
    implicit none
    integer :: i

    i = 1
    do while (i <= 10)
        if (i == 5) cycle
        print *, i
        i = i + 1
    end do
end program test_do_while_cycle
