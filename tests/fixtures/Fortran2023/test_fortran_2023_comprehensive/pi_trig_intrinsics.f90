program pi_trig_intrinsics
    implicit none
    real :: x, y, result

    x = 0.5

    result = sinpi(x)
    result = cospi(x)
    result = tanpi(x)

    result = asinpi(0.5)
    result = acospi(0.5)
    result = atanpi(1.0)

    x = 1.0
    y = 1.0
    result = atanpi(y, x)
    result = atan2pi(y, x)

end program pi_trig_intrinsics
