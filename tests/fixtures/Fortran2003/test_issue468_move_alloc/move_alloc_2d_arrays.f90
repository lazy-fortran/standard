! ISO/IEC 1539-1:2004 Section 13.7.80 - MOVE_ALLOC intrinsic subroutine
! MOVE_ALLOC with 2D arrays
program test_move_alloc_2d
    implicit none
    real, allocatable :: old_matrix(:,:), new_matrix(:,:)
    integer :: m, n

    m = 100
    n = 200
    allocate(old_matrix(m, n))
    call move_alloc(old_matrix, new_matrix)

    if (allocated(new_matrix)) then
        deallocate(new_matrix)
    end if
end program test_move_alloc_2d
