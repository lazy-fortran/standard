module do_concurrent_basic
    implicit none
contains
    subroutine basic_loop()
        integer :: i
        real :: a(100)
        do concurrent (i = 1:100)
            a(i) = real(i)
        end do
    end subroutine basic_loop
end module do_concurrent_basic
