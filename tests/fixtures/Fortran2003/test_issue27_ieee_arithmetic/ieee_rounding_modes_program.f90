program test
    use, intrinsic :: ieee_arithmetic
    implicit none

    call ieee_set_rounding_mode(ieee_nearest)
    call ieee_set_rounding_mode(ieee_to_zero)
    call ieee_set_rounding_mode(ieee_up)
    call ieee_set_rounding_mode(ieee_down)
end program test

