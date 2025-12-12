! Invalid: ALLOCATE SOURCE= with TYPE-SPEC
! Violates ISO/IEC 1539-1:2004 C631
! C631: If SOURCE= is present, TYPE-SPEC must not appear
program allocate_source_with_type
  implicit none

  type :: my_type
    integer :: value
  end type my_type

  class(*), allocatable :: poly_obj
  type(my_type) :: template

  ! INVALID: TYPE-SPEC (derived_type_spec) with SOURCE=
  ! C631 constraint violation
  allocate(my_type :: poly_obj, source=template)

end program allocate_source_with_type
