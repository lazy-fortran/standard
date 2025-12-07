module pdt_ctor_nested
  implicit none

  type :: inner_t(k)
    integer, kind :: k
    integer :: value
  end type inner_t

  type :: outer_t(k)
    integer, kind :: k
    type(inner_t(k)) :: inner
  end type outer_t

contains

  subroutine demo
    type(outer_t(4)) :: o

    ! Nested PDT structure constructors in component position
    o = outer_t(4)( inner = inner_t(4)(42) )
  end subroutine demo

end module pdt_ctor_nested

