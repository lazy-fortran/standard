program test
    use, intrinsic :: ieee_exceptions
    implicit none
    logical :: overflow_flag

    call ieee_get_flag(ieee_overflow, overflow_flag)
    call ieee_set_flag(ieee_overflow, .false.)

    if (overflow_flag) then
        print *, 'Overflow detected'
    end if
end program test

