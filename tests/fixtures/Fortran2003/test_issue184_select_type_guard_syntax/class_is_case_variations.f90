! Issue #184 - CLASS IS guard with case variations
! Tests case-insensitivity of the IS identifier in class guards
module class_is_case_test_mod
  implicit none
  type :: base_t
  end type base_t
end module class_is_case_test_mod

program class_is_case_test
  use class_is_case_test_mod
  implicit none
  class(base_t), allocatable :: obj

  allocate(base_t :: obj)

  select type (obj)
  class is (base_t)
    print *, 'lowercase is'
  end select

  select type (obj)
  class IS (base_t)
    print *, 'uppercase IS'
  end select

  select type (obj)
  CLASS Is (base_t)
    print *, 'mixed CLASS Is'
  end select

  select type (obj)
  ClAsS iS (base_t)
    print *, 'alternating case'
  end select

end program class_is_case_test
