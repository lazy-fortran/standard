module c_funcs
    use iso_c_binding
    implicit none

contains

    integer(c_int) function add_ints(a, b) bind(c, name="add_ints")
        integer(c_int), value :: a, b
        add_ints = a + b
    end function add_ints

end module c_funcs

