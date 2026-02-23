module bad_traits_stmt
    implicit none

    abstract interface :: INumeric
        integer | real
    end interface INumeric

    implements INumeric integer
    end implements integer
end module bad_traits_stmt
