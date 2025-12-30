! Test: Infer mode with function definitions
! Reference: LFortran infer mode (--infer flag)

! Bare use statement
use iso_fortran_env, only: dp => real64

! Bare declaration
real(dp) :: x, y

! Function definition at global scope
function square(n) result(res)
    real(dp), intent(in) :: n
    real(dp) :: res
    res = n * n
end function square

! Bare statements using the function
x = 5.0_dp
y = square(x)

print *, "x =", x
print *, "x^2 =", y
