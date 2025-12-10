program test_associated
    ! Test ASSOCIATED intrinsic function per ISO/IEC 1539:1991 Section 13.8.6
    ! ASSOCIATED(POINTER [, TARGET]) - Pointer association status inquiry

    integer, pointer :: ptr1
    integer, pointer :: ptr2
    integer, target :: target_var
    logical :: is_associated

    ! Initialize pointers to null
    nullify(ptr1)
    nullify(ptr2)

    ! Test 1: ASSOCIATED with 1 argument - checks if pointer is associated
    is_associated = associated(ptr1)

    ! Test 2: ASSOCIATED with 2 arguments - checks if pointer is associated with target
    target_var = 42
    ptr2 => target_var
    is_associated = associated(ptr2, target_var)

    ! Test 3: ASSOCIATED in IF statement - common usage pattern
    if (associated(ptr2)) then
        print *, "Pointer is associated"
    end if

    ! Test 4: ASSOCIATED in expression
    if (.not. associated(ptr1) .and. associated(ptr2)) then
        print *, "ptr1 is null but ptr2 is associated"
    end if

    ! Test 5: Multiple ASSOCIATED calls in same statement
    is_associated = associated(ptr1) .or. associated(ptr2, target_var)

end program test_associated
