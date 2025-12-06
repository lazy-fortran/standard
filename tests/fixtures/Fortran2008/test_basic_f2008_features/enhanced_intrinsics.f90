module test_intrinsics
    implicit none
    integer :: n, int_result
contains
    subroutine test_functions()
        real :: x, result_val
        x = 1.0
        result_val = bessel_j0(x)
        result_val = bessel_j1(x)
        result_val = bessel_jn(n, x)
        result_val = bessel_y0(x)
        result_val = bessel_y1(x)
        result_val = bessel_yn(n, x)
        result_val = erf(x)
        result_val = erfc(x)
        result_val = gamma(x)
        result_val = log_gamma(x)
        result_val = norm2((/ x, 2.0*x /))
        int_result = findloc((/ x, 2.0*x /), x)
        int_result = storage_size(x)
        int_result = parity((/ .true., .false., .true. /))
    end subroutine test_functions
end module test_intrinsics

