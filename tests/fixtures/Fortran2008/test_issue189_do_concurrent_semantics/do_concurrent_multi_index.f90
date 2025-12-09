module do_concurrent_multi
    implicit none
contains
    subroutine multi_index_loop()
        integer :: i, j
        real :: matrix(10, 10)
        do concurrent (i = 1:10, j = 1:10)
            matrix(i, j) = real(i * j)
        end do
    end subroutine multi_index_loop
end module do_concurrent_multi
