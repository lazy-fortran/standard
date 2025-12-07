module pdt_t_module
  implicit none

  type :: t(k)
    integer, kind :: k = 0
    integer :: value
  end type t

contains

  subroutine use_t
    type(t) :: foo
    ! Type parameter set with keyword, component by position
    foo = t(k=0)(42)
  end subroutine use_t

end module pdt_t_module

