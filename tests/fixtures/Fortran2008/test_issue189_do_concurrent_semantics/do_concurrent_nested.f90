module do_concurrent_nested
    implicit none
contains
    subroutine nested_loops()
        integer :: i, j
        real :: a(10, 10)
        do concurrent (i = 1:10)
            do concurrent (j = 1:10)
                a(i, j) = real(i + j)
            end do
        end do
    end subroutine nested_loops
end module do_concurrent_nested
