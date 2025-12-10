program test_count_reduction
    implicit none
    integer :: arr(5)
    integer :: cnt
    logical :: mask(5)
    arr(1) = 1
    arr(2) = 2
    arr(3) = 3
    arr(4) = 4
    arr(5) = 5
    mask(1) = .false.
    mask(2) = .false.
    mask(3) = .true.
    mask(4) = .true.
    mask(5) = .true.
    cnt = count(mask)
    print *, "Count:", cnt
end program test_count_reduction
