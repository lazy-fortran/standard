! Invalid: POINTER and TARGET attributes together
! VIOLATION: ISO/IEC 1539:1991 Section 5.2.7-5.2.8 (E676-001)
! POINTER and TARGET attributes are mutually exclusive.
!
! This fixture demonstrates INCORRECT attribute combination.
! Semantic validator should report E676-001 error.

program test_pointer_target_invalid
  implicit none

  ! INVALID: variable cannot be both POINTER and TARGET
  integer, pointer, target :: x

  ! This is a semantic constraint violation - attributes are mutually exclusive

end program test_pointer_target_invalid
