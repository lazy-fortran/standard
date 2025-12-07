program bad_guard
  implicit none
  class(*), allocatable :: obj

  select type (obj)
  type (integer)
     print *, 'missing is'
  class default
     print *, 'default'
  end select

end program bad_guard

