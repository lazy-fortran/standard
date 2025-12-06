module proc_ptr_mod
    implicit none

    abstract interface
        function math_func(x) result(y)
            real, intent(in) :: x
            real :: y
        end function math_func
    end interface

    procedure(math_func), pointer :: func_ptr => null()

contains

    function square(x) result(y)
        real, intent(in) :: x
        real :: y
        y = x**2
    end function square

    function cube(x) result(y)
        real, intent(in) :: x
        real :: y
        y = x**3
    end function cube

    subroutine test_proc_pointers()
        real :: value = 2.0

        ! Point to square function
        func_ptr => square
        print *, 'Square:', func_ptr(value)

        ! Point to cube function
        func_ptr => cube
        print *, 'Cube:', func_ptr(value)
    end subroutine test_proc_pointers

end module proc_ptr_mod

