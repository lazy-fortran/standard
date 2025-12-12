! Valid: POINTER array with deferred-shape specification
! ISO/IEC 1539:1991 (WG5 N692) Section 5.2.7
! POINTER arrays must have deferred-shape specs (:)
!
! This fixture demonstrates correct POINTER array declaration
! with deferred-shape specification allowing dynamic allocation.

program test_pointer_deferred
  implicit none
  integer, pointer :: p(:)
  integer, pointer :: q(:,:)

  allocate(p(10))
  allocate(q(5, 5))

  p(1) = 42
  q(1, 1) = 99

  deallocate(p)
  deallocate(q)

end program test_pointer_deferred
