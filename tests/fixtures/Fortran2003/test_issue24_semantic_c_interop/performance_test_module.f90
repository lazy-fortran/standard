module performance_test
    use iso_c_binding
    implicit none

contains

    subroutine simple_proc() bind(c)
        integer(c_int) :: result
        result = 1
    end subroutine simple_proc

    subroutine another_proc() bind(c)
        real(c_double) :: value
        value = 2.0
    end subroutine another_proc

end module performance_test

