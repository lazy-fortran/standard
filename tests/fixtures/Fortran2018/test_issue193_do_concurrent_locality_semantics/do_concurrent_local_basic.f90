module do_concurrent_local_basic
    ! ISO/IEC 1539-1:2018 Section 11.1.7.2 - DO CONCURRENT with LOCAL
    implicit none
contains
    subroutine compute_squares(a, b)
        integer, intent(in) :: a(:)
        integer, intent(out) :: b(:)
        integer :: i
        integer :: temp

        do concurrent(i=1:size(a)) local(temp)
            temp = a(i)
            b(i) = temp*temp
        end do
    end subroutine compute_squares
end module do_concurrent_local_basic
