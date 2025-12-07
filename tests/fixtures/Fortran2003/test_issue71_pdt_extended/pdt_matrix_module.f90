module pdt_matrix
  implicit none

  type :: matrix_t(k, m, n)
    integer, kind :: k = 4
    integer, len  :: m, n
    real(k)       :: data(m, n)
  end type matrix_t

end module pdt_matrix

