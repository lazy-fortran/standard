! Invalid: Duplicate ERRMSG= in ALLOCATE
! Violates ISO/IEC 1539-1:2004 C630
! C630: Each alloc-opt may appear at most once
program allocate_dup_errmsg
  implicit none

  integer, allocatable :: arr(:)
  character(len=256) :: msg1, msg2

  ! INVALID: ERRMSG= appears twice
  ! C630 constraint violation
  allocate(arr(100), errmsg=msg1, errmsg=msg2)

end program allocate_dup_errmsg
