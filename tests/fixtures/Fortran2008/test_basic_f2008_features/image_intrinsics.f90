program coarray_test
    integer :: my_img, total_imgs
    my_img = this_image()
    total_imgs = num_images()
    print *, 'Image', my_img, 'of', total_imgs
end program coarray_test

