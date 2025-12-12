! Invalid: POINTER array with explicit-shape specification
! VIOLATION: ISO/IEC 1539:1991 Section 5.2.7 (E676-006)
! POINTER arrays must have deferred-shape specs (:), not explicit-shape (n)
!
! This fixture demonstrates INCORRECT POINTER array declaration
! with explicit-shape specification, which violates standard constraints.
! Semantic validator should report E676-006 error.

program test_pointer_explicit_invalid
  implicit none
  integer, pointer :: p(10)  ! INVALID: explicit (10), should be deferred (:)

  ! This should not parse/compile
  allocate(p(1:10))
  p(1) = 42

end program test_pointer_explicit_invalid
