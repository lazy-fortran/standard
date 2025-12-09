module test_hypot
    implicit none
contains
    subroutine test_hypot_function()
        real :: x, y, result_val
        x = 3.0
        y = 4.0
        result_val = hypot(x, y)
        result_val = hypot(3.0, 4.0)
        result_val = hypot(x, 4.0)
    end subroutine test_hypot_function
end module test_hypot
