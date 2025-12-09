module do_concurrent_mask
    implicit none
contains
    subroutine masked_loop()
        integer :: i
        real :: a(100), b(100)
        b = 1.0
        do concurrent (i = 1:100, b(i) > 0.5)
            a(i) = real(i)
        end do
    end subroutine masked_loop
end module do_concurrent_mask
