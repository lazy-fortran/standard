module test_mod
    abstract interface
        function func_interface(x) result(y)
            real, intent(in) :: x
            real :: y
        end function
    end interface

    type :: math_t
        procedure(func_interface), pointer, nopass :: operation
    end type math_t
end module test_mod

