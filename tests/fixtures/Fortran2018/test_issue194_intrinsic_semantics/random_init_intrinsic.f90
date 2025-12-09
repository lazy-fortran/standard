program random_init_intrinsic
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none
    real(dp) :: x

    call random_init(repeatable=.true., image_distinct=.false.)
    call random_number(x)
end program random_init_intrinsic
