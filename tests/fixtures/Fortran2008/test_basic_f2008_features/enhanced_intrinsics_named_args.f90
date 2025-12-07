module test_intrinsics_named_args
    implicit none
    integer :: n, idx
    real :: x, y, result_val
contains
    subroutine exercise_intrinsics_named()
        real :: arr(3)

        arr = (/ x, 2.0*x, 3.0*x /)

        ! Bessel functions with positional and named arguments
        result_val = bessel_j0(x)
        result_val = bessel_j1(x)
        result_val = bessel_jn(n, x)
        result_val = bessel_jn(n = n, x = x)

        ! Scalar math intrinsics with named arguments
        result_val = erf(x = x)
        result_val = erfc(x = x)
        result_val = gamma(x)
        result_val = log_gamma(x = x)

        ! Array-valued intrinsics with keyword arguments
        result_val = norm2(array = arr)
        idx = findloc(array = arr, value = y, dim = 1)
        idx = findloc(array = arr, value = y, dim = 1, back = .true.)
        idx = storage_size(array = arr)
        idx = storage_size(array = arr, kind = kind(idx))
        idx = parity(mask = (/ .true., .false., .true. /))
    end subroutine exercise_intrinsics_named
end module test_intrinsics_named_args

