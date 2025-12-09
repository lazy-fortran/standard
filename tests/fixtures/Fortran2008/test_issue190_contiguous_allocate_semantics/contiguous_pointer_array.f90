module contiguous_pointer_test
    implicit none
    real, contiguous, pointer :: arr_ptr(:)
contains
    subroutine set_target(target_arr)
        real, target, intent(inout) :: target_arr(:)
        arr_ptr => target_arr
    end subroutine set_target
end module contiguous_pointer_test
