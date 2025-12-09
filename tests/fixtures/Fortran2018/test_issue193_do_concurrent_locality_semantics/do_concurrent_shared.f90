module do_concurrent_shared
    ! ISO/IEC 1539-1:2018 Section 11.1.7.2 - DO CONCURRENT with SHARED
    implicit none
    integer :: global_size = 100
contains
    subroutine scale_array(a, factor)
        real, intent(inout) :: a(:)
        real, intent(in) :: factor
        integer :: i

        do concurrent(i=1:global_size) shared(factor, global_size)
            a(i) = a(i)*factor
        end do
    end subroutine scale_array
end module do_concurrent_shared
