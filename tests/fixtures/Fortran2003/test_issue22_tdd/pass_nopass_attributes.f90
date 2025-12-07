module test_mod
    type :: wrapper_t
    contains
        procedure, pass :: method_with_pass
        procedure, nopass :: static_method
    end type wrapper_t
end module test_mod

