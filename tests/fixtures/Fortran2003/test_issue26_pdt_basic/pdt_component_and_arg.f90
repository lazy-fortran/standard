module test
    type :: matrix(k, n, m)
        integer, kind :: k = 4
        integer, len :: n, m
        real(k) :: data(n, m)
    end type matrix

    type :: container_t
        type(matrix(8, 3, 3)) :: mat
    end type container_t

contains

    subroutine use_matrix(arg)
        type(matrix(8, 3, 3)), intent(inout) :: arg
        arg%data = 0.0
    end subroutine use_matrix

end module test

