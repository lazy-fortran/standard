module math_utils
    implicit none
    public :: add_numbers
    private :: helper_function

contains

    function add_numbers(a, b) result(sum)
        integer, intent(in) :: a, b
        integer :: sum
        sum = a + b
    end function add_numbers

    function helper_function() result(x)
        integer :: x
        x = 42
    end function helper_function
end module math_utils

