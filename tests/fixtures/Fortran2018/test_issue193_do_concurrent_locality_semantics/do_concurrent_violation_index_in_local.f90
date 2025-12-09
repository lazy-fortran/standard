module do_concurrent_violation_index_local
    ! ISO/IEC 1539-1:2018 Section 11.1.7.4 violation test
    ! Index variable shall not appear in locality specifier
    implicit none
contains
    subroutine bad_index_local(a)
        integer, intent(inout) :: a(:)
        integer :: i

        do concurrent(i=1:10) local(i)
            a(i) = i*2
        end do
    end subroutine bad_index_local
end module do_concurrent_violation_index_local
