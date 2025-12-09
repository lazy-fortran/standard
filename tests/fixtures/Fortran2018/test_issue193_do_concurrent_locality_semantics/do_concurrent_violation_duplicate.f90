module do_concurrent_violation_duplicate
    ! ISO/IEC 1539-1:2018 Section 11.1.7.4 violation test
    ! Variable shall not appear in more than one locality specifier
    implicit none
contains
    subroutine bad_duplicate(a)
        integer, intent(inout) :: a(:)
        integer :: i
        integer :: temp

        do concurrent(i=1:10) local(temp) shared(temp)
            temp = a(i)
            a(i) = temp + 1
        end do
    end subroutine bad_duplicate
end module do_concurrent_violation_duplicate
