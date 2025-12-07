program fortran95_features
    implicit none
    integer, parameter :: n = 5
    integer :: matrix(n,n), vector(n)

    ! FORALL construct (F95 feature)
    forall (i = 1:n, j = 1:n, i /= j)
        matrix(i,j) = i * j
    end forall

    ! Enhanced WHERE with multiple ELSEWHERE
    where (matrix > 10)
        matrix = matrix / 2
    elsewhere (matrix > 5)
        matrix = matrix - 1
    elsewhere
        matrix = 0
    end where

end program fortran95_features

