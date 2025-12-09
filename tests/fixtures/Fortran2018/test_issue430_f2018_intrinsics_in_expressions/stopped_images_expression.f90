program test_stopped_images_expression
    ! ISO/IEC 1539-1:2018 Section 16.9.182: STOPPED_IMAGES
    ! Test STOPPED_IMAGES intrinsic in expression context
    implicit none
    integer, allocatable :: stopped_ids(:)

    ! STOPPED_IMAGES must parse as primary in expression
    stopped_ids = STOPPED_IMAGES()

    if (size(STOPPED_IMAGES()) > 0) then
        print *, "Some images have stopped"
    end if

end program test_stopped_images_expression
