! Invalid: ALLOCATE SOURCE= with multiple objects
! Violates ISO/IEC 1539-1:2004 C631
! C631: If SOURCE= is present, ALLOCATION-LIST must contain exactly one
!       allocate-object
program allocate_source_multiple
  implicit none

  integer, allocatable :: a(:), b(:)
  integer :: template(10)

  ! INVALID: Multiple objects with SOURCE=
  ! C631 constraint violation
  allocate(a, b, source=template)

end program allocate_source_multiple
