module math_lib
    implicit none

    integer, parameter :: pi_approx = 3

contains

    subroutine add(a, b, c)
        integer, intent(in) :: a, b
        integer, intent(out) :: c
        c = a + b
    end subroutine add

end module math_lib
