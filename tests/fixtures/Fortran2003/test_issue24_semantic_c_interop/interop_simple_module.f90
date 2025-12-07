module interop
    use iso_c_binding
    implicit none

contains

    subroutine simple_func() bind(c, name="func")
        integer(c_int) :: result
        result = 42
    end subroutine simple_func

end module interop

