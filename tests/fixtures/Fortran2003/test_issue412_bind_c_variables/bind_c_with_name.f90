! ISO/IEC 1539-1:2004 Section 15.3.4
! BIND(C) with NAME specifier for C interop
program test_bind_c_with_name
    use, intrinsic :: iso_c_binding
    implicit none

    ! Variables with C binding and explicit C names
    integer(c_int), bind(c, name="my_counter") :: counter
    real(c_double), bind(c, name="global_value") :: value
    integer(c_int), bind(c, name="array_data") :: data_array(10)

    counter = 42
    value = 3.14d0
    data_array(1) = 1
end program test_bind_c_with_name
