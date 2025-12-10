! Comprehensive test for all F2003 OOP and memory intrinsics from issue #429
! Tests EXTENDS_TYPE_OF, SAME_TYPE_AS, MOVE_ALLOC, and NEW_LINE

module f2003_intrinsics_all
  implicit none

  type :: base_t
    integer :: value
  end type base_t

  type, extends(base_t) :: derived_t
    real :: extra
  end type derived_t

contains

  ! EXTENDS_TYPE_OF test
  subroutine test_extends()
    type(base_t) :: b
    type(derived_t) :: d
    logical :: result

    result = extends_type_of(d, b)
    if (result) then
      print *, "EXTENDS_TYPE_OF test: PASS"
    else
      print *, "EXTENDS_TYPE_OF test: FAIL"
    end if
  end subroutine test_extends

  ! SAME_TYPE_AS test
  subroutine test_same_type()
    type(base_t) :: b1, b2
    type(derived_t) :: d
    logical :: same, different

    same = same_type_as(b1, b2)
    different = same_type_as(b1, d)

    if (same .and. .not. different) then
      print *, "SAME_TYPE_AS test: PASS"
    else
      print *, "SAME_TYPE_AS test: FAIL"
    end if
  end subroutine test_same_type

  ! MOVE_ALLOC test
  subroutine test_move()
    integer, allocatable :: src(:), dst(:)
    integer :: i

    allocate(src(5))
    do i = 1, 5
      src(i) = i
    end do

    call move_alloc(src, dst)

    if (allocated(dst) .and. .not. allocated(src)) then
      print *, "MOVE_ALLOC test: PASS"
    else
      print *, "MOVE_ALLOC test: FAIL"
    end if

    deallocate(dst)
  end subroutine test_move

  ! NEW_LINE test
  subroutine test_newline()
    character(len=1) :: nl
    integer :: code

    nl = new_line('a')
    code = iachar(nl)

    if (code == 10) then
      print *, "NEW_LINE test: PASS"
    else
      print *, "NEW_LINE test: FAIL - got code:", code
    end if
  end subroutine test_newline

end module f2003_intrinsics_all

program test_all_f2003_intrinsics
  use f2003_intrinsics_all
  implicit none

  print *, "Running F2003 intrinsic function tests..."
  call test_extends()
  call test_same_type()
  call test_move()
  call test_newline()
  print *, "All tests completed."

end program test_all_f2003_intrinsics
