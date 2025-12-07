module pdt_x_ctor
  implicit none

  type :: x_t(n)
    integer, len :: n
    integer      :: v(n)
    character(len=8) :: label(n)
  end type x_t

contains

  subroutine init_x
    type(x_t(1)) :: a
    type(x_t(2)) :: b

    ! Structure constructors with array components
    a = x_t(1)( [1], ["a1"] )
    b = x_t(2)( [1,2], ["b1","b2"] )
  end subroutine init_x

end module pdt_x_ctor

