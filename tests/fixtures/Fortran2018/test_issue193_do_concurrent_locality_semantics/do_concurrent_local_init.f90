module do_concurrent_local_init
    ! ISO/IEC 1539-1:2018 Section 11.1.7.2 - DO CONCURRENT with LOCAL_INIT
    implicit none
contains
    subroutine accumulate(a, b)
        integer, intent(in) :: a(:)
        integer, intent(out) :: b(:)
        integer :: i
        integer :: accum

        do concurrent(i=1:size(a)) local_init(accum)
            accum = accum + a(i)
            b(i) = accum
        end do
    end subroutine accumulate
end module do_concurrent_local_init
