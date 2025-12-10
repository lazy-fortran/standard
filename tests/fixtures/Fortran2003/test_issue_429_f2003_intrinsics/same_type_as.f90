! Test: SAME_TYPE_AS intrinsic function (ISO/IEC 1539-1:2004 Section 13.7.102)
! This is an inquiry function returning true if A and B have the same dynamic type.
! Essential for polymorphic type comparison.

module same_type_mod
  implicit none

  type :: base_t
    integer :: value
  end type base_t

  type, extends(base_t) :: derived_t
    real :: extra
  end type derived_t

  type, extends(derived_t) :: extended_derived_t
    character(len=10) :: name
  end type extended_derived_t

contains

  ! Test 1: Compare two polymorphic objects
  logical function is_same_type(a, b)
    class(base_t), intent(in) :: a
    class(base_t), intent(in) :: b
    is_same_type = same_type_as(a, b)
  end function is_same_type

  ! Test 2: Direct comparison in conditional
  subroutine compare_types(a, b)
    class(base_t), intent(in) :: a
    class(base_t), intent(in) :: b
    if (same_type_as(a, b)) then
      print *, "Types are the same"
    else
      print *, "Types are different"
    end if
  end subroutine compare_types

end module same_type_mod

program test_same_type_as
  use same_type_mod
  implicit none

  type(base_t) :: b1, b2
  type(derived_t) :: d1, d2
  type(extended_derived_t) :: ed

  ! Test same type (base)
  if (same_type_as(b1, b2)) then
    print *, "Same base type: PASS"
  else
    print *, "Same base type: FAIL"
  end if

  ! Test same type (derived)
  if (same_type_as(d1, d2)) then
    print *, "Same derived type: PASS"
  else
    print *, "Same derived type: FAIL"
  end if

  ! Test different types
  if (.not. same_type_as(b1, d1)) then
    print *, "Different types: PASS"
  else
    print *, "Different types: FAIL"
  end if

  ! Test extended derived types
  if (.not. same_type_as(d1, ed)) then
    print *, "Extended vs derived: PASS"
  else
    print *, "Extended vs derived: FAIL"
  end if

end program test_same_type_as
