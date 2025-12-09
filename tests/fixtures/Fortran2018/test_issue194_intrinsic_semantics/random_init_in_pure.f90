module random_init_violation
    implicit none
contains
    pure subroutine invalid_random_init()
        call random_init(repeatable=.true., image_distinct=.false.)
    end subroutine invalid_random_init
end module random_init_violation
