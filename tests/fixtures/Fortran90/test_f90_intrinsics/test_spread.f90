program test_spread
    implicit none
    integer :: source(3)
    source(1) = 1
    source(2) = 2
    source(3) = 3
    call print_spread(source)
end program test_spread

subroutine print_spread(s)
    implicit none
    integer, intent(in) :: s(:)
    integer :: result(3,4)
    result = spread(s, dim=2, ncopies=4)
    print *, "Spread result"
end subroutine print_spread
