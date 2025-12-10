program test_transpose
    implicit none
    real :: matrix(2,3), transposed(3,2)
    real :: mat_vals(6)
    integer :: dims(2)
    mat_vals(1) = 1.0
    mat_vals(2) = 2.0
    mat_vals(3) = 3.0
    mat_vals(4) = 4.0
    mat_vals(5) = 5.0
    mat_vals(6) = 6.0
    dims(1) = 2
    dims(2) = 3
    matrix = reshape(mat_vals, dims)
    transposed = transpose(matrix)
    print *, "Transpose result"
end program test_transpose
