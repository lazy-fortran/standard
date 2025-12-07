program test
    use, intrinsic :: ieee_arithmetic
    implicit none
    real :: x, y
    logical :: is_nan, is_finite

    x = ieee_value(x, ieee_positive_inf)
    y = ieee_value(y, ieee_quiet_nan)

    is_nan = ieee_is_nan(x)
    is_finite = ieee_is_finite(y)

    if (is_nan) then
        print *, 'Value is NaN'
    end if

    if (.not. is_finite) then
        print *, 'Value is not finite'
    end if
end program test

