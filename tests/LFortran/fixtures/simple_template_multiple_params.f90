! Test: Simple template with multiple type parameters (J3 24-107r1)
! Reference: J3/24-107r1 - Multiple deferred arguments

function convert{T, U}(x) result(y)
    type, deferred :: T
    type, deferred :: U
    type(T), intent(in) :: x
    type(U) :: y
    y = x
end function convert
