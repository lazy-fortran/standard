module oscillator_synthesis
    use, intrinsic :: iso_fortran_env, only: real64
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
    implicit none
    private
    public :: rhs
contains
    function rhs(q, p, m) result(values)
        real(real64), intent(in) :: q, p, m
        real(real64) :: values(2)

        if (.not. ieee_is_finite(q)) error stop 'oscillator.input.q'
        if (.not. ieee_is_finite(p)) error stop 'oscillator.input.p'
        if (.not. ieee_is_finite(m)) error stop 'oscillator.input.m'
        if (.not. (m > 0.0_real64)) error stop 'oscillator.assume.1'
        values(1) = p / m
        values(2) = -q
    end function rhs
end module oscillator_synthesis

program check_oscillator_twin
    use, intrinsic :: iso_fortran_env, only: real64
    use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan, &
        ieee_positive_inf
    use oscillator_synthesis, only: rhs
    implicit none
    real(real64) :: values(2), bad_input
    character(len=32) :: mode

    call get_command_argument(1, mode)
    select case (trim(mode))
    case ('zero')
        values = rhs(1.0_real64, 2.0_real64, 0.0_real64)
        error stop 'zero mass accepted'
    case ('negative')
        values = rhs(1.0_real64, 2.0_real64, -1.0_real64)
        error stop 'negative mass accepted'
    case ('nan')
        bad_input = ieee_value(0.0_real64, ieee_quiet_nan)
        values = rhs(bad_input, 2.0_real64, 1.0_real64)
        error stop 'nonfinite position accepted'
    case ('infinite')
        bad_input = ieee_value(0.0_real64, ieee_positive_inf)
        values = rhs(1.0_real64, bad_input, 1.0_real64)
        error stop 'nonfinite momentum accepted'
    case ('')
        values = rhs(3.0_real64, 8.0_real64, 2.0_real64)
        if (any(abs(values - [4.0_real64, -3.0_real64]) > 0.0_real64)) &
            error stop 'point 1'
        values = rhs(-2.0_real64, -6.0_real64, 3.0_real64)
        if (any(abs(values - [-2.0_real64, 2.0_real64]) > 0.0_real64)) &
            error stop 'point 2'
        values = rhs(0.0_real64, 0.0_real64, 0.5_real64)
        if (any(abs(values) > 0.0_real64)) error stop 'point 3'
        print '(a)', 'OK synthesis scalar twin'
    case default
        error stop 'unknown twin mode'
    end select
end program check_oscillator_twin
