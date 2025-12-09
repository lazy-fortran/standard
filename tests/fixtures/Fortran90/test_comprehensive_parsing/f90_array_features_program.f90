program f90_array_features
    implicit none
    integer, parameter :: n = 5
    integer :: i, j
    integer :: matrix(n,n), vector(n)

    ! Use DO loops instead of FORALL (FORALL is F95)
    do i = 1, n
        do j = 1, n
            if (i /= j) then
                matrix(i,j) = i * j
            end if
        end do
    end do

    ! WHERE construct with ELSEWHERE (valid F90)
    where (matrix > 10)
        matrix = matrix / 2
    elsewhere
        matrix = 0
    end where

end program f90_array_features

