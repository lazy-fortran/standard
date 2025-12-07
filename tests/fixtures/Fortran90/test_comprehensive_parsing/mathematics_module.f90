module mathematics
    implicit none
    public :: add, multiply
    private :: internal_helper
contains
    function add(a, b) result(sum)
        integer, intent(in) :: a, b
        integer :: sum
        sum = a + b
    end function add
end module mathematics

