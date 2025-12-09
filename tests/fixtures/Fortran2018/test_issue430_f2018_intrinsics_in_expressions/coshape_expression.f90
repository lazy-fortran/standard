program test_coshape_expression
    ! ISO/IEC 1539-1:2018 Section 16.9.52: COSHAPE
    ! Test COSHAPE intrinsic in expression context
    implicit none
    integer :: arr[*]
    integer, allocatable :: shape_result(:)

    ! COSHAPE must parse as primary in expression
    shape_result = COSHAPE(arr)

    if (size(COSHAPE(arr)) > 0) then
        print *, "Coarray shape determined"
    end if

end program test_coshape_expression
