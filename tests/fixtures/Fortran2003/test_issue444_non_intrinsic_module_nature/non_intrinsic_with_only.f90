! Test Fortran 2003 NON_INTRINSIC with ONLY clause (ISO/IEC 1539-1:2004 R1109)
! NON_INTRINSIC restricts import to specific entities

module math_lib
    implicit none
    real, parameter :: euler = 2.71828
    real, parameter :: sqrt2 = 1.41421
contains
    function square(x) result(y)
        real, intent(in) :: x
        real :: y
        y = x * x
    end function square
end module math_lib

program test_non_intrinsic_only
    use, non_intrinsic :: math_lib, only: euler, square
    implicit none
    real :: result
    result = square(3.0)
    print *, "e = ", euler
    print *, "square(3) = ", result
end program test_non_intrinsic_only
