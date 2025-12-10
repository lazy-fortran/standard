program test_out_of_range_expression
    ! ISO/IEC 1539-1:2018 Section 16.9.140: OUT_OF_RANGE
    ! Test OUT_OF_RANGE intrinsic in expression context
    implicit none
    real :: value
    integer :: ivalue

    value = 1.5e38

    ! OUT_OF_RANGE must parse as primary in expression
    if (OUT_OF_RANGE(value, ivalue)) then
        print *, "Value is out of range for integer"
    end if

end program test_out_of_range_expression
