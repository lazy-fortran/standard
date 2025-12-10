program test_reduce_expression
    ! ISO/IEC 1539-1:2018 Section 16.9.161: REDUCE
    ! Test REDUCE intrinsic in expression context
    implicit none
    integer :: arr(10), result_val

    arr = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

    ! REDUCE must parse as primary in expression (with operation_spec)
    ! Note: REDUCE requires operation-spec which is part of the grammar
    result_val = REDUCE(arr, my_add_op)

end program test_reduce_expression
