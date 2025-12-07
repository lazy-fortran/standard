module ieee_demo
    use, intrinsic :: ieee_exceptions
    use, intrinsic :: ieee_arithmetic
    use, intrinsic :: ieee_features
    implicit none

contains
    subroutine safe_calculation()
        real :: x, y, result
        logical :: overflow_flag, underflow_flag
        logical :: is_valid

        ! Save exception state
        call ieee_get_flag(ieee_overflow, overflow_flag)
        call ieee_get_flag(ieee_underflow, underflow_flag)

        ! Clear exceptions
        call ieee_set_flag(ieee_overflow, .false.)
        call ieee_set_flag(ieee_underflow, .false.)

        ! Set rounding mode
        call ieee_set_rounding_mode(ieee_nearest)

        ! Perform calculation
        x = huge(1.0)
        y = 2.0
        result = x * y

        ! Check for special values
        is_valid = ieee_is_finite(result) .and. .not. ieee_is_nan(result)

        if (.not. is_valid) then
            ! Handle infinite or NaN result
            result = ieee_value(result, ieee_positive_inf)
        end if

        ! Check for exceptions
        call ieee_get_flag(ieee_overflow, overflow_flag)
        if (overflow_flag) then
            print *, 'Overflow occurred during calculation'
        end if

        ! Restore exception state if needed
        call ieee_set_flag(ieee_overflow, overflow_flag)
    end subroutine safe_calculation
end module ieee_demo

