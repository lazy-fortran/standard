! Test: MOVE_ALLOC intrinsic subroutine (ISO/IEC 1539-1:2004 Section 13.7.81)
! Transfers allocation from FROM to TO without copying data.
! Essential for efficient memory management with allocatables.

program test_move_alloc
  implicit none

  integer, allocatable :: arr(:)
  integer, allocatable :: result(:)
  integer :: i

  ! Allocate source array
  allocate(arr(10))
  do i = 1, 10
    arr(i) = i * 10
  end do

  ! Move allocation test 1: positional arguments
  call move_alloc(arr, result)

  ! Verify result has data
  if (allocated(result)) then
    print *, "Move allocation successful: PASS"
  else
    print *, "Move allocation failed: FAIL"
  end if

  ! Verify source is deallocated
  if (.not. allocated(arr)) then
    print *, "Source deallocated: PASS"
  else
    print *, "Source deallocated: FAIL"
  end if

  deallocate(result)

end program test_move_alloc
