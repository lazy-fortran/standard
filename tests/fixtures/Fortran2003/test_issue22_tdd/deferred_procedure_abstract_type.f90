module test_mod
    type, abstract :: shape_t
    contains
        procedure(area_interface), deferred :: area_method
    end type shape_t
end module test_mod

