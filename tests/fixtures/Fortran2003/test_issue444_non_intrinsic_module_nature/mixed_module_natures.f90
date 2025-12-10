! Test mixing INTRINSIC and NON_INTRINSIC modules (ISO/IEC 1539-1:2004 R1110)
! Same program uses both intrinsic and user-defined modules with explicit module nature

module user_module
    implicit none
    integer, parameter :: MY_CONSTANT = 42
end module user_module

program test_mixed_natures
    use, intrinsic :: iso_c_binding, only: c_int, c_ptr
    use, non_intrinsic :: user_module, only: MY_CONSTANT
    implicit none

    integer(c_int) :: c_value
    c_value = MY_CONSTANT
    print *, "C interop value: ", c_value
end program test_mixed_natures
