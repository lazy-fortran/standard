! Test: EXTENDS_TYPE_OF intrinsic function (ISO/IEC 1539-1:2004 Section 13.7.42)
! This is an inquiry function returning true if A's dynamic type is an extension
! of MOLD's type. Essential for polymorphic type checking.

module type_inquiry_mod
  implicit none

  type :: base_t
    integer :: value
  end type base_t

  type, extends(base_t) :: derived_t
    real :: extra
  end type derived_t

contains

  ! Test 1: Check if obj is extended type
  logical function is_extended_type(obj, mold)
    class(base_t), intent(in) :: obj
    class(base_t), intent(in) :: mold
    is_extended_type = extends_type_of(obj, mold)
  end function is_extended_type

  ! Test 2: Direct call in expression
  subroutine check_polymorphic(obj)
    class(base_t), intent(in) :: obj
    if (extends_type_of(obj, base_t())) then
      print *, "Object extends base type"
    end if
  end subroutine check_polymorphic

end module type_inquiry_mod

program test_extends_type_of
  use type_inquiry_mod
  implicit none

  type(base_t) :: b
  type(derived_t) :: d

  ! Test extends_type_of with derived type
  if (extends_type_of(d, b)) then
    print *, "Derived extends base: PASS"
  else
    print *, "Derived extends base: FAIL"
  end if

  ! Test extends_type_of with same type
  if (.not. extends_type_of(b, b)) then
    print *, "Base same type: PASS"
  else
    print *, "Base same type: FAIL"
  end if

end program test_extends_type_of
