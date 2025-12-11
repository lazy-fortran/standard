! Valid derived type default initialization
! ISO/IEC 1539-1:1997 Section 4.4.1 - derived type components with constant defaults

program derived_type_init
  implicit none

  ! Derived type with default component initialization
  type :: point_t
    real :: x = 0.0
    real :: y = 0.0
    character(len=10) :: label = "origin"
  end type

  type :: box_t
    integer :: width = 10
    integer :: height = 20
    integer :: depth = 5
  end type

  ! Structure constructor with constant expressions
  type(point_t) :: origin
  type(box_t) :: default_box

  ! Initialize from structure constructor
  type(point_t) :: custom = point_t(1.0, 2.0, "custom")

end program derived_type_init
