program test_enum_bind_c
  implicit none

  enum, bind(c)
    enumerator :: red = 1, green = 1 + 1, blue = 3
  end enum

  integer :: color
  color = red

end program test_enum_bind_c
