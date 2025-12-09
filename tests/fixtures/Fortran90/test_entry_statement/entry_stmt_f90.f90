! ENTRY statement test fixture - ISO/IEC 1539:1991 Section 12.5.2.4
! Tests R1219: entry-stmt is ENTRY entry-name [ ( [ dummy-arg-list ] ) [ suffix ] ]
program test_entry_program
    implicit none
    real :: result_val

    call compute_ops(2.0, 3.0, result_val)
    call compute_product(2.0, 3.0, result_val)
    call reset_value(result_val)

end program test_entry_program

! Subroutine with multiple ENTRY points
subroutine compute_ops(a, b, result)
    implicit none
    real, intent(in) :: a, b
    real, intent(out) :: result

    result = a + b
    return

    ! ENTRY with different parameter names
entry compute_product(x, y, result)
    result = x * y
    return

    ! ENTRY with no argument list
entry reset_value(result)
    result = 0.0
    return

end subroutine compute_ops

! Function with ENTRY and RESULT clause (F90 enhancement)
function power_base(x, n) result(res)
    implicit none
    real, intent(in) :: x
    integer, intent(in) :: n
    real :: res
    integer :: i

    res = 1.0
    do i = 1, n
        res = res * x
    end do
    return

    ! ENTRY with RESULT clause - F90 specific
entry square_value(x) result(res)
    res = x * x
    return

    ! ENTRY with empty argument list and RESULT
entry get_unity() result(res)
    res = 1.0
    return

end function power_base

! Subroutine with alternate return specifier in ENTRY
subroutine safe_divide(a, b, quotient, *)
    implicit none
    real, intent(in) :: a, b
    real, intent(out) :: quotient

    if (b == 0.0) return 1
    quotient = a / b
    return

    ! ENTRY with alternate return specifier (*)
entry checked_reciprocal(val, reciprocal, *)
    if (val == 0.0) return 1
    reciprocal = 1.0 / val
    return

end subroutine safe_divide
