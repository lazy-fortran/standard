program test_product
    implicit none
    integer :: arr(5)
    integer :: prod
    arr(1) = 1
    arr(2) = 2
    arr(3) = 3
    arr(4) = 4
    arr(5) = 5
    prod = product(arr)
    print *, "Product:", prod
end program test_product
