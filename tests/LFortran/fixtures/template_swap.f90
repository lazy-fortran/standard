! Test: Basic TEMPLATE construct with swap procedure
! Reference: J3/24-107r1 TEMPLATE syntax
! Note: Uses curly braces {} for deferred argument list per J3 spec

template swap_t{T}
    type, deferred :: T
contains
    subroutine swap(x, y)
        type(T), intent(inout) :: x, y
        type(T) :: tmp
        tmp = x
        x = y
        y = tmp
    end subroutine swap
end template swap_t
