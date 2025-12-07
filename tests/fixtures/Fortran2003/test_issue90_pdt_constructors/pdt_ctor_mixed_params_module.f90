module pdt_ctor_mixed
  implicit none

  type :: t(k, n)
    integer, kind :: k
    integer, len  :: n
    integer       :: val(n)
  end type t

contains

  subroutine demo
    type(t(8,3)) :: a
    type(t(8,3)) :: b

    ! Mixed positional and keyword type parameters with positional component
    a = t(8, n=3)( [1,2,3] )

    ! Mixed keyword and positional type parameters with named component
    b = t(k=8, 3)( val = [1,2,3] )
  end subroutine demo

end module pdt_ctor_mixed

