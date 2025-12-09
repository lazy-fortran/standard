program out_of_range_intrinsic
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none
    real(dp) :: x
    integer :: mold
    logical :: is_out

    x = 1.0d38
    is_out = out_of_range(x, mold)
    is_out = out_of_range(x, mold, round=.true.)
end program out_of_range_intrinsic
