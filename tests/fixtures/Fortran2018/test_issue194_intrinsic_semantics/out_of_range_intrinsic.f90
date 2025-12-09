program out_of_range_intrinsic
    implicit none
    real :: x
    integer :: mold
    logical :: is_out

    x = 1.0e38
    is_out = out_of_range(x, mold)
    is_out = out_of_range(x, mold, round=.true.)
end program out_of_range_intrinsic
