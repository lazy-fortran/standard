! Test: Simple template function (J3 24-107r1)
! Reference: J3/24-107r1 - Simple template procedures with deferred args in name
!
! Syntax: function template-name { deferred-arg-list } (dummy-args) result(res)
!         This is shorthand for a full TEMPLATE construct

function mysum{T}(x, y) result(z)
    type, deferred :: T
    type(T), intent(in) :: x, y
    type(T) :: z
    z = x + y
end function mysum
