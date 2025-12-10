! ENTRY statement test fixture for Fortran 95
! ISO/IEC 1539-1:1997 Section 12.5.4
! Tests R1234: entry-stmt is ENTRY entry-name [ ( [ dummy-arg-list ] ) [ suffix ] ]
!
! Fortran 95 ENTRY statements can appear in:
! - Subroutines
! - Functions
! - Functions with RESULT clause (F90 feature)
! And they are inherited from Fortran 77/90 and marked as obsolescent in F95.
!
! This fixture verifies that ENTRY statements parse correctly when using
! executable_stmt_f95, which previously did not include entry_stmt_f90.

program test_entry_f95
    implicit none
    real :: x, y, res_val

    ! Test subroutine with ENTRY
    call multiply(3.0, 4.0, res_val)
    call add(5.0, 6.0, res_val)

    ! Test function with ENTRY
    x = compute_sum(1.0, 2.0)
    y = compute_product(3.0, 4.0)

end program test_entry_f95

! Subroutine with multiple ENTRY points
subroutine multiply(a, b, res)
    implicit none
    real, intent(in) :: a, b
    real, intent(out) :: res

    res = a * b
    return

    ! ENTRY point for addition
entry add(a, b, res)
    res = a + b
    return

end subroutine multiply

! Function with ENTRY and F95 pure keyword
pure function compute_sum(x, y) result(total)
    implicit none
    real, intent(in) :: x, y
    real :: total

    total = x + y
    return

    ! ENTRY for product computation
entry compute_product(x, y) result(total)
    total = x * y
    return

end function compute_sum

! Subroutine with ENTRY in F95 context (can have FORALL in execution part)
subroutine process_array(n, arr, out_val)
    implicit none
    integer, intent(in) :: n
    real, intent(inout) :: arr(n)
    real, intent(out) :: out_val

    ! F95 feature: FORALL in same executable construct
    forall (i = 1:n)
        arr(i) = arr(i) * 2.0
    end forall
    out_val = sum(arr)
    return

entry clear_array(n, arr, out_val)
    ! Another F95 FORALL usage
    forall (j = 1:n)
        arr(j) = 0.0
    end forall
    out_val = 0.0
    return

end subroutine process_array
