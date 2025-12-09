program test_atomic_intrinsics
    use iso_fortran_env
    implicit none
    integer(atomic_int_kind) :: counter[*]
    integer :: val, stat_var

    call atomic_define(counter, 0)
    call atomic_ref(val, counter)

    call atomic_define(counter, 42)
    call atomic_ref(val, counter)

    call atomic_define(counter, val + 1)
    call atomic_ref(val, counter)

    call atomic_define(counter, 0, stat=stat_var)
    call atomic_ref(val, counter, stat=stat_var)
end program test_atomic_intrinsics
