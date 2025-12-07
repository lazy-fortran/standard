program poly_nested
  implicit none
  class(*), allocatable :: obj

  select type (obj)
  type is (integer)
    print *, 'integer'
  class is (circle_t)
    print *, 'circle'
    select type (obj2 => obj)
    class is (circle_t)
      print *, 'nested circle'
    class default
      print *, 'nested default'
    end select
  class default
    print *, 'outer default'
  end select

end program poly_nested

