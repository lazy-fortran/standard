module coarray_section_reference
    implicit none
    real :: a(:)[*]

contains

    subroutine update_section
        integer :: i

        a(:)[this_image()] = 0.0
    end subroutine update_section

end module coarray_section_reference

