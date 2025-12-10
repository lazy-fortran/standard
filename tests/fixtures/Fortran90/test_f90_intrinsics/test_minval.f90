program test_minval
    implicit none
    integer :: arr(5)
    integer :: min_val
    arr(1) = 3
    arr(2) = 1
    arr(3) = 4
    arr(4) = 1
    arr(5) = 5
    min_val = minval(arr)
    print *, "Minimum value:", min_val
end program test_minval
