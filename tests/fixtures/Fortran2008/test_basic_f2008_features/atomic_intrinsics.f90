program test_atomic_intrinsics
    use iso_fortran_env
    implicit none
    integer(atomic_int_kind) :: counter[*], flags[*]
    integer :: val, stat_var, old_val, compare_val, new_val
    logical :: success

    ! ATOMIC_DEFINE and ATOMIC_REF (F2008 Section 13.7.19-20)
    call atomic_define(counter, 0)
    call atomic_ref(val, counter)

    call atomic_define(counter, 42)
    call atomic_ref(val, counter)

    call atomic_define(counter, val + 1)
    call atomic_ref(val, counter)

    call atomic_define(counter, 0, stat=stat_var)
    call atomic_ref(val, counter, stat=stat_var)

    ! ATOMIC_CAS - Atomic compare-and-swap (F2008 Section 13.7.21)
    call atomic_cas(counter, old_val, 42, 100)
    call atomic_cas(counter, old_val, 100, 0, stat=stat_var)

    ! ATOMIC_ADD - Atomic addition (F2008 Section 13.7.15)
    call atomic_add(counter, 1)
    call atomic_add(counter, 5, stat=stat_var)

    ! ATOMIC_AND - Atomic bitwise AND (F2008 Section 13.7.16)
    call atomic_and(flags, z'FF00')
    call atomic_and(flags, z'00FF', stat=stat_var)

    ! ATOMIC_OR - Atomic bitwise OR (F2008 Section 13.7.17)
    call atomic_or(flags, z'0F0F')
    call atomic_or(flags, z'F0F0', stat=stat_var)

    ! ATOMIC_XOR - Atomic bitwise XOR (F2008 Section 13.7.18)
    call atomic_xor(flags, z'FFFF')
    call atomic_xor(flags, z'AAAA', stat=stat_var)
end program test_atomic_intrinsics
