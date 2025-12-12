! Valid ALLOCATE SOURCE= usage (ISO/IEC 1539-1:2004 Section 6.3.1)
! Tests that correct SOURCE= usage does not produce errors
program allocate_source_valid
  implicit none

  integer, allocatable :: arr(:)
  integer, allocatable :: copy(:)
  integer :: template(10)
  integer :: istat

  ! Valid: single object with SOURCE=
  allocate(arr, source=template)

  ! Valid: allocatable with SOURCE= and STAT=
  allocate(copy, source=arr, stat=istat)

end program allocate_source_valid
