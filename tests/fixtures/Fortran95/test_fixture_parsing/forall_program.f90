program forall_example
    implicit none
    integer :: a(10), i
    do i = 1, 10
        a(i) = i
    end do
    forall (i = 1:10)
        a(i) = a(i) * 2
    end forall
end program forall_example
