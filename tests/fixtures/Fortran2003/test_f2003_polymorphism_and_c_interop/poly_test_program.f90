program poly_test
  implicit none
  class(*), allocatable :: obj

  select type (obj)
  type is (integer)
    print *, 'int'
  class default
    print *, 'other'
  end select

end program poly_test

