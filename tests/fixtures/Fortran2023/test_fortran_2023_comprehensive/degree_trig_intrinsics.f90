program degree_trig_intrinsics
    implicit none
    real :: angle_deg, result, x, y

    angle_deg = 45.0

    result = sind(angle_deg)
    result = cosd(angle_deg)
    result = tand(angle_deg)

    result = asind(0.5)
    result = acosd(0.5)
    result = atand(1.0)

    x = 1.0
    y = 1.0
    result = atand(y, x)
    result = atan2d(y, x)

end program degree_trig_intrinsics
