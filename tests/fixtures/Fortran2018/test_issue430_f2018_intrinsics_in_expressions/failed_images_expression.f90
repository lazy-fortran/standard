program test_failed_images_expression
    ! ISO/IEC 1539-1:2018 Section 16.9.73: FAILED_IMAGES
    ! Test FAILED_IMAGES intrinsic in expression context
    implicit none
    integer, allocatable :: failed_ids(:)

    ! FAILED_IMAGES must parse as primary in expression
    failed_ids = FAILED_IMAGES()

    if (size(FAILED_IMAGES()) > 0) then
        print *, "Some images have failed"
    end if

end program test_failed_images_expression
