! ISO/IEC 1539-1:2004 Section 13.7.80 - MOVE_ALLOC intrinsic subroutine
! MOVE_ALLOC within a loop
program test_move_alloc_loop
    implicit none
    integer, allocatable :: temp(:), result(:)
    integer :: i

    do i = 1, 5
        allocate(temp(10*i))
        call move_alloc(temp, result)
        if (allocated(result)) then
            deallocate(result)
        end if
    end do
end program test_move_alloc_loop
