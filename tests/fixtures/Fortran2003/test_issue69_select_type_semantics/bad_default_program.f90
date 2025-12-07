program bad_default
  implicit none
  class(*), allocatable :: obj

  select type (obj)
  class default (integer)
     print *, 'illegal'
  end select

end program bad_default

