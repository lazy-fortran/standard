program test_maxval
    implicit none
    integer :: arr(5)
    integer :: max_val
    arr(1) = 3
    arr(2) = 1
    arr(3) = 4
    arr(4) = 1
    arr(5) = 5
    max_val = maxval(arr)
    print *, "Maximum value:", max_val
end program test_maxval
