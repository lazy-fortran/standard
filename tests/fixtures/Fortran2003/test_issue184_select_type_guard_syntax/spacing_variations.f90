! Issue #184 - TYPE IS and CLASS IS guards with spacing variations
! Tests that varied whitespace between keywords is handled correctly
module spacing_test_mod
  implicit none
  type :: point_t
    real :: x, y
  end type point_t
end module spacing_test_mod

program spacing_test
  use spacing_test_mod
  implicit none
  class(*), allocatable :: obj_any
  class(point_t), allocatable :: obj_pt

  allocate(obj_any, source=3.14)
  allocate(point_t :: obj_pt)

  ! Single space (standard)
  select type (obj_any)
  type is (real)
    print *, 'single space'
  class default
    print *, 'default'
  end select

  ! Multiple spaces between TYPE and IS
  select type (obj_any)
  type    is (real)
    print *, 'multiple spaces'
  end select

  ! Multiple spaces between IS and parenthesis
  select type (obj_any)
  type is    (real)
    print *, 'spaces before paren'
  end select

  ! Tab-like spacing (multiple spaces throughout)
  select type (obj_any)
  type     is     (real)
    print *, 'heavy spacing'
  end select

  ! CLASS IS with varied spacing
  select type (obj_pt)
  class    is    (point_t)
    print *, 'class with spacing'
  end select

end program spacing_test
