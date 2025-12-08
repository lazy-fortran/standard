! Issue #184 - Verify IS can still be used as an identifier
! The grammar treats IS as IDENTIFIER in type guards, so IS must remain
! a valid identifier name in other contexts
program is_as_identifier
  implicit none
  class(*), allocatable :: obj
  integer :: is
  real :: is_value
  logical :: is_valid

  is = 42
  is_value = 3.14
  is_valid = .true.

  allocate(obj, source=is)

  select type (obj)
  type is (integer)
    ! Use variables named with is to verify no conflict
    print *, 'value:', is
    print *, 'is_value:', is_value
    print *, 'is_valid:', is_valid
  class default
    is = 0
  end select

  print *, 'final is:', is

end program is_as_identifier
