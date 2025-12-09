program test_combined_f2018_intrinsics_expressions
    ! ISO/IEC 1539-1:2018 Section 16.9: Combined F2018 intrinsic function tests
    ! Test multiple F2018 intrinsics in various expression contexts
    implicit none

    integer :: status, img_id, team_id, dim_size, arr_size
    integer, allocatable :: failed_ids(:), stopped_ids(:)
    real :: value
    integer :: ivalue
    integer :: arr[*]

    img_id = 2
    value = 1.5e38

    ! All F2018 intrinsics must parse in expression contexts
    ! (This comprehensive test exercises multiple intrinsics together)

    ! Image status functions in expressions
    status = IMAGE_STATUS(img_id)

    if (size(FAILED_IMAGES()) > 0 .and. size(STOPPED_IMAGES()) > 0) then
        print *, "Mixed image status check"
    end if

    ! Collective functions in expressions
    team_id = TEAM_NUMBER()
    dim_size = size(COSHAPE(arr))

    ! Math functions in expressions
    if (OUT_OF_RANGE(value, ivalue)) then
        print *, "Numeric range check"
    end if

end program test_combined_f2018_intrinsics_expressions
