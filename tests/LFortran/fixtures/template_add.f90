! Test: TEMPLATE with function returning type(T)
! Reference: J3/24-107r1 TEMPLATE syntax
! Note: Uses curly braces {} for deferred argument list per J3 spec

template add_t{T}
    type, deferred :: T
contains
    function add(a, b) result(res)
        type(T), intent(in) :: a, b
        type(T) :: res
        res = a + b
    end function add
end template add_t
