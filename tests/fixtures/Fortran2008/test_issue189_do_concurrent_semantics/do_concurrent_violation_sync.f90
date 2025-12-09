program do_conc_sync_violation
    implicit none
    integer :: i
    real :: a(10)[*]
    do concurrent (i = 1:10)
        a(i) = real(i)
        sync all
    end do
end program do_conc_sync_violation
