! ISO/IEC 1539-1:2004 Section 13.7.80 - MOVE_ALLOC intrinsic subroutine
! Basic MOVE_ALLOC usage
program test_move_alloc_basic
    implicit none
    integer, allocatable :: old_array(:), new_array(:)

    allocate(old_array(100))
    call move_alloc(old_array, new_array)
end program test_move_alloc_basic
