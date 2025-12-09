module do_concurrent_violation_default_none
    ! ISO/IEC 1539-1:2018 Section 11.1.7.5 violation test
    ! With DEFAULT(NONE), all variables must have locality specified
    implicit none
contains
    subroutine missing_locality(a, b)
        integer, intent(in) :: a(:)
        integer, intent(out) :: b(:)
        integer :: i

        do concurrent(i=1:10) default(none)
            b(i) = a(i) + 1
        end do
    end subroutine missing_locality
end module do_concurrent_violation_default_none
