module color_enum_module
  implicit none

  enum, bind(c)
    enumerator :: white = 0
    enumerator :: black = 1
    enumerator :: red = 2
    enumerator :: green = 3
    enumerator :: blue = 4
    enumerator :: yellow = 5
  end enum

contains

  subroutine describe_color(color_val)
    implicit none
    integer :: color_val
  end subroutine describe_color

end module color_enum_module
