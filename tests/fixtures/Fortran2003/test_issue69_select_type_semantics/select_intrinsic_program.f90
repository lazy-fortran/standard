program select_intrinsic
  implicit none
  class(*), allocatable :: obj

  select type (obj)
  type is (integer)
     print *, 'int'
  type is (real)
     print *, 'real'
  class default
     print *, 'other'
  end select

end program select_intrinsic

