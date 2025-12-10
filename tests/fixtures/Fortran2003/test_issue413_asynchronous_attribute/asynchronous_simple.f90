! Test Fortran 2003 ASYNCHRONOUS attribute (ISO/IEC 1539-1:2004 Section 5.1.2.1, R503)
program test_asynchronous_simple
    implicit none
    integer, asynchronous :: buffer(100)
    integer :: stat

    buffer = 0
end program test_asynchronous_simple
