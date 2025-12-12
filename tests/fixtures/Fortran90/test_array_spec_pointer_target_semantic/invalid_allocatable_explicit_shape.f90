! Invalid: ALLOCATABLE array with explicit-shape specification
! VIOLATION: ISO/IEC 1539:1991 Section 5.1.2.4 (E676-008)
! ALLOCATABLE arrays must have deferred-shape specs (:), not explicit-shape (n)
!
! This fixture demonstrates INCORRECT ALLOCATABLE array declaration
! with explicit-shape specification, which violates standard constraints.
! Semantic validator should report E676-008 error.

program test_allocatable_explicit_invalid
  implicit none
  integer, allocatable :: a(10)  ! INVALID: explicit (10), should be deferred (:)

  ! This declaration is invalid
  allocate(a)

end program test_allocatable_explicit_invalid
