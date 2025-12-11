module math_ops
    implicit none

    interface sqrt_generic module procedure sqrt_real, sqrt_complex
    end interface

contains

    real function sqrt_real(x)
        real, intent(in) :: x
        sqrt_real = sqrt(x)
    end function sqrt_real

    complex function sqrt_complex(z)
        complex, intent(in) :: z
        sqrt_complex = sqrt(z)
    end function sqrt_complex
end module math_ops
