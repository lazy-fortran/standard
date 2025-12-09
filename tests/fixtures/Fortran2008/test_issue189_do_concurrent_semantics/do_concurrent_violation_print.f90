program do_conc_print_violation
    implicit none
    integer :: i
    do concurrent (i = 1:10)
        print *, i
    end do
end program do_conc_print_violation
