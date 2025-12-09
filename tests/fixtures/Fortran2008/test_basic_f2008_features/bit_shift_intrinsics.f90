module test_bit_shift
    implicit none
contains
    subroutine test_shift_functions()
        integer :: i, j, result_val
        i = 16
        j = 2
        result_val = shiftl(1, 4)
        result_val = shiftr(i, j)
        result_val = shifta(i, j)
        result_val = maskl(8)
        result_val = maskr(8)
    end subroutine test_shift_functions
end module test_bit_shift
