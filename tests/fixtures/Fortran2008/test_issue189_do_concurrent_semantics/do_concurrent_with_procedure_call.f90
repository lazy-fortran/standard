program do_concurrent_proc
    implicit none
    integer :: i
    real :: a(10)
    do concurrent (i = 1:10)
        a(i) = real(i) * 2.0
    end do
end program do_concurrent_proc
