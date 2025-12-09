program test_image_status_expression
    ! ISO/IEC 1539-1:2018 Section 16.9.81: IMAGE_STATUS
    ! Test IMAGE_STATUS intrinsic in expression context
    implicit none
    integer :: status, img_id

    img_id = 2

    ! IMAGE_STATUS must parse as primary in expression
    status = IMAGE_STATUS(img_id)

    if (IMAGE_STATUS(img_id) == 0) then
        print *, "Image is executing"
    end if

end program test_image_status_expression
