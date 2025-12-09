program do_conc_stop_violation
    implicit none
    integer :: i
    real :: a(10)
    do concurrent (i = 1:10)
        a(i) = real(i)
        stop 1
    end do
end program do_conc_stop_violation
