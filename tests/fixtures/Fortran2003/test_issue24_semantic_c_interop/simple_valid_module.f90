module test
    use iso_c_binding
    implicit none

contains

    subroutine valid_proc() bind(c)
        integer(c_int) :: x
    end subroutine valid_proc

end module test

