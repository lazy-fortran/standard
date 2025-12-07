program test_prog
    abstract interface
        function func_interface(x) result(y)
            real, intent(in) :: x
            real :: y
        end function
    end interface

    procedure(func_interface), pointer :: func_ptr

    func_ptr => actual_function
end program test_prog

