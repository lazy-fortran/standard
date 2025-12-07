module c_types
    use iso_c_binding
    implicit none

    type, bind(c) :: point
        real(c_double) :: x
        real(c_double) :: y
    end type point

end module c_types

