module pdt_matrix_ctor
  implicit none

  type :: matrix_t(k, m, n)
    integer, kind :: k
    integer, len  :: m, n
    real(k)       :: data(m, n)
  end type matrix_t

contains

  subroutine build_matrix
    type(matrix_t(8,2,2)) :: a
    ! Positional type parameters followed by component values
    a = matrix_t(8,2,2)( [1.0, 2.0, 3.0, 4.0] )
  end subroutine build_matrix

end module pdt_matrix_ctor

