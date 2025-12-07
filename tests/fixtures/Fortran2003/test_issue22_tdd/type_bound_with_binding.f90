module test_mod
    type :: shape_t
    contains
        procedure :: area => area_impl
    end type shape_t
end module test_mod

