program nested_select
  implicit none
  class(*), allocatable :: obj

  select type (obj)
  class is (shape_t)
    select type (s => obj)
    type is (circle_t)
      print *, 'circle branch'
    class default
      print *, 'shape but not circle'
    end select
  class default
    print *, 'non-shape'
  end select

end program nested_select

