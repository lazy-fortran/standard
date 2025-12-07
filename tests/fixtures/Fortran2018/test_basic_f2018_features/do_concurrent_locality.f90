module do_concurrent_locality
    ! ISO/IEC 1539-1:2018 Section 11.1.7.2 - DO CONCURRENT with locality specs
    implicit none
    integer :: n = 10
contains
    subroutine update_positive(a)
        integer, intent(inout) :: a(:)
        integer :: i
        integer :: temp

        do concurrent (i = 1:n, a(i) > 0) local(temp) shared(n)
            temp = a(i)
            a(i) = temp + 1
        end do
    end subroutine update_positive
end module do_concurrent_locality
