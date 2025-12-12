! Invalid: Duplicate STAT= in ALLOCATE
! Violates ISO/IEC 1539-1:2004 C630
! C630: Each alloc-opt may appear at most once
program allocate_dup_stat
  implicit none

  integer, allocatable :: arr(:)
  integer :: stat1, stat2

  ! INVALID: STAT= appears twice
  ! C630 constraint violation
  allocate(arr(100), stat=stat1, stat=stat2)

end program allocate_dup_stat
