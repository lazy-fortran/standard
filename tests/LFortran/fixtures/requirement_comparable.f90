! Test: REQUIREMENT construct for type constraints
! Reference: J3/24-107r1 REQUIREMENT syntax
! Note: Uses curly braces {} for deferred argument list per J3 spec

requirement comparable{T, less_than}
    type, deferred :: T
    interface
        pure logical function less_than(a, b)
            type(T), intent(in) :: a, b
        end function less_than
    end interface
end requirement comparable
