! Issue #184 - TYPE IS guard with case variations
! Tests case-insensitivity of the IS identifier in type guards
program type_is_case_test
  implicit none
  class(*), allocatable :: obj

  allocate(obj, source=42)

  select type (obj)
  type is (integer)
    print *, 'lowercase is'
  end select

  select type (obj)
  type IS (integer)
    print *, 'uppercase IS'
  end select

  select type (obj)
  TYPE Is (integer)
    print *, 'mixed TYPE Is'
  end select

  select type (obj)
  TyPe iS (integer)
    print *, 'alternating case'
  end select

end program type_is_case_test
