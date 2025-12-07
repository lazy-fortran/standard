module test_mod
    abstract interface
        function func_interface(x) result(y)
            real, intent(in) :: x
            real :: y
        end function
    end interface

    procedure(func_interface), pointer :: func_ptr
end module test_mod

