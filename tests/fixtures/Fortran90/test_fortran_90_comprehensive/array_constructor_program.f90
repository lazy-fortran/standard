program test_arrays
    integer :: numbers(5)
    integer :: matrix(2,2)
    integer :: shape_arr(2)
    integer :: data_arr(4)

    ! Simple array constructor
    numbers = (/1, 2, 3, 4, 5/)

    ! RESHAPE with intermediate variables
    data_arr = (/1, 2, 3, 4/)
    shape_arr = (/2, 2/)
    matrix = reshape(data_arr, shape_arr)
end program

