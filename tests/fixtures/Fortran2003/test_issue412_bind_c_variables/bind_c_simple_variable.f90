! ISO/IEC 1539-1:2004 Section 15.3.4
! BIND(C) attribute for variables
program test_bind_c_simple
    use, intrinsic :: iso_c_binding
    implicit none

    ! Simple BIND(C) on module variable (not in module, but same syntax applies)
    integer(c_int), bind(c) :: counter
    real(c_double), bind(c) :: value

    counter = 42
    value = 3.14d0
end program test_bind_c_simple
