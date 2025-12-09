module do_concurrent_default_none
    ! ISO/IEC 1539-1:2018 Section 11.1.7.5 - DO CONCURRENT with DEFAULT(NONE)
    implicit none
contains
    subroutine strict_compute(a, b, n)
        real, intent(in) :: a(:)
        real, intent(out) :: b(:)
        integer, intent(in) :: n
        integer :: i
        real :: temp

        do concurrent(i=1:n) local(temp) shared(a, b, n) default(none)
            temp = a(i)
            b(i) = temp*2.0
        end do
    end subroutine strict_compute
end module do_concurrent_default_none
