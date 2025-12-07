program test_prog
    abstract interface
        function func_interface(x) result(y)
            real, intent(in) :: x
            real :: y
        end function
    end interface

    procedure(func_interface), pointer :: func_ptr
    real :: result, value

    result = func_ptr(value)
end program test_prog

