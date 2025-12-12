! Valid: ALLOCATABLE array with deferred-shape specification
! ISO/IEC 1539:1991 (WG5 N692) Section 5.1.2.4
! ALLOCATABLE arrays must have deferred-shape specs (:)
!
! This fixture demonstrates correct ALLOCATABLE array declaration
! with deferred-shape specification enabling dynamic memory allocation.

program test_allocatable_deferred
  implicit none
  integer, allocatable :: a(:)
  integer, allocatable :: b(:,:,:)

  allocate(a(100))
  allocate(b(10, 20, 30))

  a(1) = 42
  b(1, 1, 1) = 99

  deallocate(a)
  deallocate(b)

end program test_allocatable_deferred
