! Issue #184 - Complete matrix of guard forms
! Tests all valid type guard forms with typical usage patterns
module guard_matrix_mod
  implicit none

  type :: shape_t
    character(len=20) :: name
  end type shape_t

  type, extends(shape_t) :: circle_t
    real :: radius
  end type circle_t

  type, extends(shape_t) :: rectangle_t
    real :: width, height
  end type rectangle_t

end module guard_matrix_mod

program guard_matrix_test
  use guard_matrix_mod
  implicit none
  class(*), allocatable :: any_obj
  class(shape_t), allocatable :: shape_obj

  ! Test with intrinsic types
  allocate(any_obj, source=100)

  select type (any_obj)
  type is (integer)
    print *, 'integer'
  type is (real)
    print *, 'real'
  type is (logical)
    print *, 'logical'
  type is (character(*))
    print *, 'character'
  class default
    print *, 'unknown'
  end select

  deallocate(any_obj)

  ! Test with derived types and selector rename
  allocate(circle_t :: shape_obj)
  select type (shape_obj)
  type is (shape_t)
    print *, 'exact shape_t'
  class is (shape_t)
    print *, 'class shape_t or derived'
  end select

  ! Test with selector rename using =>
  select type (s => shape_obj)
  type is (circle_t)
    print *, 'circle with radius'
  type is (rectangle_t)
    print *, 'rectangle with dims'
  class is (shape_t)
    print *, 'some shape:', s%name
  class default
    print *, 'not a shape'
  end select

  deallocate(shape_obj)

  ! Test CLASS(*) unlimited polymorphic
  allocate(any_obj, source='Hello')
  select type (u => any_obj)
  type is (integer)
    print *, 'int:', u
  type is (character(*))
    print *, 'char:', u
  class default
    print *, 'other type'
  end select

end program guard_matrix_test
