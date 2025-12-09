module do_concurrent_violation_shared_assign
    ! ISO/IEC 1539-1:2018 Section 11.1.7.5 warning test
    ! Assigning to SHARED variable may violate iteration independence
    implicit none
contains
    subroutine shared_assign_warning(a)
        integer, intent(inout) :: a(:)
        integer :: i
        integer :: counter

        counter = 0
        do concurrent(i=1:10) shared(counter)
            counter = counter + 1
            a(i) = counter
        end do
    end subroutine shared_assign_warning
end module do_concurrent_violation_shared_assign
