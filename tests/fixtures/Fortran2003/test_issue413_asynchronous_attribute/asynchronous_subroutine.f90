! Test Fortran 2003 ASYNCHRONOUS attribute in subroutine (ISO/IEC 1539-1:2004 Section 5.1.2.1)
subroutine process_async_data(buffer, unit)
    integer, asynchronous, intent(inout) :: buffer
    integer, intent(in) :: unit
end subroutine process_async_data

program test_async_subroutine
    implicit none
    integer, asynchronous, allocatable :: data_buffer
    integer :: unit_id

    allocate(data_buffer)

    open(newunit=unit_id, file='async_test.dat')

    close(unit_id)
    deallocate(data_buffer)
end program test_async_subroutine
