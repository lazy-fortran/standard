module test
    type :: matrix(k)
        integer, kind :: k = 4
    end type matrix

    type(matrix) :: my_matrix
end module test

