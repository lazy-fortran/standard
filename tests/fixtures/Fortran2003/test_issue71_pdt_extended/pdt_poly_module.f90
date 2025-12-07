module pdt_poly
  implicit none

  type :: poly_t(k, n)
    integer, kind :: k = 8
    integer, len  :: n
    real(k)       :: coeffs(0:n)
  end type poly_t

  type(poly_t(8,:)), allocatable :: polys(:)
  type(poly_t(*,10))             :: default_poly

end module pdt_poly

