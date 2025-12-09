module do_concurrent_combined_locality
    ! ISO/IEC 1539-1:2018 Section 11.1.7.2 - Combined locality specifiers
    implicit none
contains
    subroutine process_data(input, output, n)
        real, intent(in) :: input(:)
        real, intent(out) :: output(:)
        integer, intent(in) :: n
        integer :: i
        real :: work_val
        real :: scale_factor

        scale_factor = 2.5
        do concurrent(i=1:n) local(work_val) shared(scale_factor, n)
            work_val = input(i)*scale_factor
            output(i) = work_val + 1.0
        end do
    end subroutine process_data
end module do_concurrent_combined_locality
