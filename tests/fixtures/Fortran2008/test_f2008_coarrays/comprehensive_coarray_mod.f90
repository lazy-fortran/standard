module comprehensive_coarray_mod
    implicit none
    integer :: counter[*]
    real    :: results(100)[*]

contains

    subroutine parallel_work()
        integer :: my_img, total_imgs

        my_img = this_image()
        total_imgs = num_images()

        counter[my_img] = my_img * 10

        sync all

        if (my_img == 1) then
            print *, 'All images synchronized'
        end if

        sync memory

    end subroutine parallel_work

end module comprehensive_coarray_mod

