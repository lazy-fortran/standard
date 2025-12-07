program test_logical_ops
    use, intrinsic :: ieee_arithmetic
    implicit none
    real :: x, y
    logical :: result1, result2, result3

    x = 1.0
    y = 2.0

    ! Test all three logical operators in IEEE context
    result1 = ieee_is_finite(x) .and. ieee_is_finite(y)
    result2 = ieee_is_nan(x) .or. ieee_is_nan(y)
    result3 = .not. ieee_is_infinite(x)
end program test_logical_ops

