! Fortran 90: EXIT in DO WHILE construct
! ISO/IEC 1539:1991 Section 8.1.4.4.4
program test_do_while_exit
    implicit none
    integer :: i

    i = 1
    do while (i <= 10)
        if (i == 5) exit
        print *, i
        i = i + 1
    end do
end program test_do_while_exit
