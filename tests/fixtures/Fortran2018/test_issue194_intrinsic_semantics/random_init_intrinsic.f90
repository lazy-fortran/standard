program random_init_intrinsic
    implicit none
    real :: x

    call random_init(repeatable=.true., image_distinct=.false.)
    call random_number(x)
end program random_init_intrinsic
