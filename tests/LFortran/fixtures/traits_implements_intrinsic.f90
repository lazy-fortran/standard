module trait_intrinsic_impl
    implicit none

    abstract interface :: INumeric
        integer | real
    end interface INumeric

    implements INumeric :: integer
    end implements integer
end module trait_intrinsic_impl
