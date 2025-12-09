program free_form_features
    implicit none
    integer :: i, j, matrix(5,5)
    ! Array constructor (F90 parenthesis form)
    integer, parameter :: small_array(3) = (/1, 2, 3/)

    ! Free-form comment
    do i = 1, 5
        do j = 1, 5
            matrix(i,j) = i + j  ! End-of-line comment
        end do
    end do

    ! WHERE array syntax
    where (matrix > 5)
        matrix = matrix * 2
    end where

end program free_form_features

