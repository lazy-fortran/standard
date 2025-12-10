program test_merge
    implicit none
    integer :: a(3), b(3), result(3)
    logical :: mask(3)
    a(1) = 1
    a(2) = 2
    a(3) = 3
    b(1) = 10
    b(2) = 20
    b(3) = 30
    mask(1) = .true.
    mask(2) = .false.
    mask(3) = .true.
    result = merge(a, b, mask)
    print *, "Merge result"
end program test_merge
