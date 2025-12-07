module test_mod
    abstract interface
        function func_interface(x) result(y)
            real, intent(in) :: x
            real :: y
        end function
        subroutine sub_interface(x)
            real, intent(inout) :: x
        end subroutine
    end interface

    procedure(func_interface), pointer :: func_ptr1, func_ptr2
    procedure(sub_interface), pointer :: sub_ptr
end module test_mod

