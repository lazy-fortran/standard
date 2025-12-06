module c_interop_mod
    use iso_c_binding
    implicit none

    ! C-interoperable derived type
    type, bind(c) :: c_struct_t
        integer(c_int) :: i
        real(c_float) :: x
        character(c_char) :: name(20)
    end type c_struct_t

    ! Interface to C function
    interface
        function c_function(x) bind(c, name=\"my_c_function\")
            use iso_c_binding
            real(c_float), value :: x
            real(c_float) :: c_function
        end function c_function
    end interface

contains

    subroutine test_c_interop()
        type(c_struct_t) :: my_struct
        real(c_float) :: result

        my_struct%i = 42
        my_struct%x = 3.14
        my_struct%name = c_char_\"Hello\"//c_null_char

        result = c_function(my_struct%x)
        print *, 'C function result:', result
    end subroutine test_c_interop

end module c_interop_mod

