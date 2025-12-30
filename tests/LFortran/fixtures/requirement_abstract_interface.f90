! Test: REQUIREMENT construct with abstract interface body
! Reference: J3/24-107r1 REQUIREMENT syntax
! Note: Uses curly braces {} for deferred argument list per J3 spec

requirement addable{T, add_op}
    type, deferred :: T

    ! Abstract interface body (alternative to interface block)
    pure function add_op(a, b) result(res)
        type(T), intent(in) :: a, b
        type(T) :: res
    end function add_op
end requirement addable
