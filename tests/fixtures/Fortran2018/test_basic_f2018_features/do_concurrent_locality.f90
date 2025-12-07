module do_concurrent_locality
    implicit none
    integer :: n
contains
    subroutine update_positive(a)
        integer, intent(inout) :: a(:)
        integer :: i

        do concurrent (i = 1:n, local :: i, shared :: n, a(i) > 0)
            a(i) = a(i) + 1
        end do
    end subroutine update_positive
end module do_concurrent_locality

