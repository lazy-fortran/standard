module test_mod
    type :: vector_t
    contains
        generic :: operator(+) => add_vectors
    end type vector_t
end module test_mod

