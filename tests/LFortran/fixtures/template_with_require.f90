! Test: TEMPLATE with REQUIRE constraint
! Reference: J3/24-107r1 TEMPLATE + REQUIRE syntax

template min_t(T, less_than)
    type, deferred :: T
    require :: comparable(T, less_than)
contains
    function min_value(a, b) result(res)
        type(T), intent(in) :: a, b
        type(T) :: res
        if (less_than(a, b)) then
            res = a
        else
            res = b
        end if
    end function min_value
end template min_t
