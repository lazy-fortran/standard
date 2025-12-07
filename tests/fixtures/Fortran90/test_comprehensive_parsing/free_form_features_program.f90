program free_form_features
    implicit none
    integer :: i, j, matrix(5,5)

    ! Free-form comment
    do i = 1, 5
        do j = 1, 5
            matrix(i,j) = i + j  ! End-of-line comment
        end do
    end do

    ! Array constructor
    integer, parameter :: small_array(3) = [1, 2, 3]

    ! Modern array syntax
    where (matrix > 5)
        matrix = matrix * 2
    end where

end program free_form_features

