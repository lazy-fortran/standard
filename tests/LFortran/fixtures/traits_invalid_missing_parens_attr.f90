module bad_traits_attr
    implicit none

    abstract interface :: INumeric
        integer | real
    end interface INumeric

    type, implements INumeric :: bad_type
    end type bad_type
end module bad_traits_attr
