module test_bit_reduction
    implicit none
contains
    subroutine test_bitwise_reduction_functions()
        integer :: arr(4), result_val
        logical :: mask_arr(4)
        arr = [15, 7, 3, 1]
        mask_arr = [.true., .true., .false., .true.]
        result_val = iall(arr)
        result_val = iany(arr)
        result_val = iparity(arr)
        result_val = iall(arr, dim=1)
        result_val = iany(arr, dim=1)
        result_val = iparity(arr, dim=1)
        result_val = iall(arr, mask=mask_arr)
        result_val = iany(arr, mask=mask_arr)
        result_val = iparity(arr, mask=mask_arr)
    end subroutine test_bitwise_reduction_functions
end module test_bit_reduction
