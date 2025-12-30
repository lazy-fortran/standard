! Test: Simple template subroutine (J3 24-107r1)
! Reference: J3/24-107r1 - Simple template procedures with deferred args in name
!
! Syntax: subroutine template-name { deferred-arg-list } (dummy-args)
!         This is shorthand for a full TEMPLATE construct

subroutine swap{T}(x, y)
    type, deferred :: T
    type(T), intent(inout) :: x, y
    type(T) :: tmp
    tmp = x
    x = y
    y = tmp
end subroutine swap
