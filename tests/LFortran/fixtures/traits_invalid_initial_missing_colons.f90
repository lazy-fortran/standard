module bad_traits_initial
    implicit none

    type, sealed :: bad_type
    contains
        initial init
    end type bad_type
end module bad_traits_initial
