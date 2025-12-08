program where_example
    implicit none
    real :: a(10), b(10)
    integer :: i
    do i = 1, 10
        a(i) = 1.0 * i
    end do
    where (a > 5.0)
        b = a * 2.0
    elsewhere
        b = a
    end where
end program where_example
