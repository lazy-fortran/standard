! ISO/IEC 1539-1:2004 Section 15.3.4
! BIND(C) combined with other attributes
program test_bind_c_with_attributes
    use, intrinsic :: iso_c_binding
    implicit none

    ! BIND(C) with other attributes
    real(c_double), target, bind(c, name="target_var") :: target_value
    integer(c_int), volatile, bind(c, name="volatile_flag") :: flag
    integer(c_int), bind(c, name="array_values") :: values(10)

    target_value = 3.14d0
    flag = 1
    values(1) = 42
end program test_bind_c_with_attributes
