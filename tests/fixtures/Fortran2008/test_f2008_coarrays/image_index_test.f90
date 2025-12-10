program test_image_index
    implicit none
    real :: x[2,3,*]
    integer :: img_idx
    integer :: cosubscripts(3)

    ! Test IMAGE_INDEX with cosubscripts array
    cosubscripts = [1, 2, 3]
    img_idx = image_index(x, cosubscripts)

    ! Test with different cosubscript values
    cosubscripts = [2, 1, 1]
    img_idx = image_index(x, cosubscripts)

    ! Test bounds checking (returns 0 if out of bounds)
    cosubscripts = [99, 99, 99]
    img_idx = image_index(x, cosubscripts)
    if (img_idx == 0) then
        print *, 'Out of bounds cosubscripts detected'
    end if

    print *, 'IMAGE_INDEX test completed'
end program test_image_index

