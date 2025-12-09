module contiguous_assumed_shape_test
    implicit none
contains
    subroutine process_contiguous(arr)
        real, contiguous, intent(inout) :: arr(:)
        integer :: i, n
        n = size(arr)
        do i = 1, n
            arr(i) = arr(i) * 2.0
        end do
    end subroutine process_contiguous

    subroutine process_2d_contiguous(matrix)
        real, contiguous, intent(in) :: matrix(:,:)
        print *, sum(matrix)
    end subroutine process_2d_contiguous
end module contiguous_assumed_shape_test
