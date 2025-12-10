program test_dot_product
    implicit none
    real :: v1(3), v2(3)
    real :: result
    v1(1) = 1.0
    v1(2) = 2.0
    v1(3) = 3.0
    v2(1) = 4.0
    v2(2) = 5.0
    v2(3) = 6.0
    result = dot_product(v1, v2)
    print *, "Dot product:", result
end program test_dot_product
