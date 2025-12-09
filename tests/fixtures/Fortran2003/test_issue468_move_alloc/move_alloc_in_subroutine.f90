! ISO/IEC 1539-1:2004 Section 13.7.80 - MOVE_ALLOC intrinsic subroutine
! MOVE_ALLOC within a subroutine
module move_alloc_mod
    implicit none
contains
    subroutine reallocate_array(old_data, new_data)
        integer, allocatable, intent(inout) :: old_data(:)
        integer, allocatable, intent(out) :: new_data(:)

        call move_alloc(old_data, new_data)
    end subroutine reallocate_array
end module move_alloc_mod
