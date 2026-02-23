module traits_demo
    implicit none

    abstract interface :: INumeric
        integer | real(8)
        function operator(+)(lhs, rhs) result(res)
            type(itself), intent(in) :: lhs, rhs
            type(itself) :: res
        end function
    end interface INumeric

    abstract interface :: IPrintable
        integer | real(8)
    end interface IPrintable

    type, sealed, implements(INumeric) :: pairwise_sum
    contains
        initial :: init
        procedure, pass :: accumulate
    end type pairwise_sum

    implements (INumeric + IPrintable) :: pairwise_sum
    end implements pairwise_sum

contains

    function accumulate{INumeric :: T}(x) result(res)
        type(T), intent(in) :: x
        type(T) :: res
        res = x
    end function accumulate

end module traits_demo
