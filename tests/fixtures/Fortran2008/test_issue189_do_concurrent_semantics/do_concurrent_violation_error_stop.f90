program do_conc_error_stop_violation
    implicit none
    integer :: i
    real :: a(10)
    do concurrent (i = 1:10)
        a(i) = real(i)
        error stop
    end do
end program do_conc_error_stop_violation
