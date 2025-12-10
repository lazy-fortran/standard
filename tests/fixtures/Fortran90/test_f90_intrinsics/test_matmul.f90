program test_matmul
    implicit none
    real :: a(2,2), b(2,2), c(2,2)
    real :: a_vals(4), b_vals(4)
    integer :: a_dims(2), b_dims(2)
    a_vals(1) = 1.0
    a_vals(2) = 2.0
    a_vals(3) = 3.0
    a_vals(4) = 4.0
    b_vals(1) = 5.0
    b_vals(2) = 6.0
    b_vals(3) = 7.0
    b_vals(4) = 8.0
    a_dims(1) = 2
    a_dims(2) = 2
    b_dims(1) = 2
    b_dims(2) = 2
    a = reshape(a_vals, a_dims)
    b = reshape(b_vals, b_dims)
    c = matmul(a, b)
    print *, "Matrix multiplication complete"
end program test_matmul
