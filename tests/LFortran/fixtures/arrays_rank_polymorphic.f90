! Array contracts proposal: rank-polymorphic shape(*) in a generic.
module norms
    implicit none
contains
    function array_norm{T}(x) result(s)
        type(T), shape(*), intent(in) :: x
        type(T) :: s
        s = sum(x * x)
    end function array_norm
end module norms
