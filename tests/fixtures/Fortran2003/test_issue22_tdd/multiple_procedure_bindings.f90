module test_mod
    type :: math_t
    contains
        procedure :: add_int, add_real
        generic :: operator(+) => add_int, add_real
    end type math_t
end module test_mod

