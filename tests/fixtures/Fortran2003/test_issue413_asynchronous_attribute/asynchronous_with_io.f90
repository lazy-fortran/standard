! Test Fortran 2003 ASYNCHRONOUS attribute with I/O (ISO/IEC 1539-1:2004 Section 5.1.2.1)
program test_asynchronous_io
    implicit none
    integer, asynchronous, allocatable :: async_buffer
    integer :: ios, unit_id

    allocate(async_buffer)

    open(newunit=unit_id, file='test.dat')

    close(unit_id)

    deallocate(async_buffer)
end program test_asynchronous_io
