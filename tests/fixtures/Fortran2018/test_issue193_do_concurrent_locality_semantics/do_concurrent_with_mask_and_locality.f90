module do_concurrent_mask_locality
    ! ISO/IEC 1539-1:2018 Section 11.1.7.2 - Mask with locality
    implicit none
contains
    subroutine filter_positives(a, b)
        integer, intent(in) :: a(:)
        integer, intent(out) :: b(:)
        integer :: i
        integer :: temp

        do concurrent(i=1:size(a), a(i) > 0) local(temp) shared(a, b)
            temp = a(i)
            b(i) = temp*2
        end do
    end subroutine filter_positives
end module do_concurrent_mask_locality
