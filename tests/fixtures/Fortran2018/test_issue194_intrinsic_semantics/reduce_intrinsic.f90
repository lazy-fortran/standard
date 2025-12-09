program reduce_intrinsic
    implicit none
    integer :: arr(10), result_val
    logical :: mask_arr(10)

    interface
        pure function add_op(a, b) result(c)
            integer, intent(in) :: a, b
            integer :: c
        end function add_op
    end interface

    arr = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    mask_arr = .true.

    result_val = reduce(arr, add_op)
    result_val = reduce(arr, add_op, dim=1)
    result_val = reduce(arr, add_op, mask=mask_arr)
end program reduce_intrinsic
