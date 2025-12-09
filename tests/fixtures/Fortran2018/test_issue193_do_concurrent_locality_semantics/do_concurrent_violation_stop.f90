module do_concurrent_violation_stop
    ! ISO/IEC 1539-1:2018 Section 11.1.7.5 violation test
    ! STOP statement prohibited in DO CONCURRENT
    implicit none
contains
    subroutine bad_stop(a)
        integer, intent(inout) :: a(:)
        integer :: i
        integer :: temp

        do concurrent(i=1:10) local(temp)
            temp = a(i)
            stop
            a(i) = temp + 1
        end do
    end subroutine bad_stop
end module do_concurrent_violation_stop
