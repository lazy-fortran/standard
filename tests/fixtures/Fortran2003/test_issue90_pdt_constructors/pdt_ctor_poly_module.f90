module pdt_ctor_poly
  implicit none

  type :: poly_t(k, n)
    integer, kind :: k
    integer, len  :: n
    real(k)       :: coeffs(0:n)
  end type poly_t

contains

  subroutine demo
    type(poly_t(8,:)), allocatable :: polys(:)
    type(poly_t(*,10))             :: default_poly

    ! PDT structure constructors in a module that also uses
    ! deferred (:) and assumed (*) type parameters.
    polys(1) = poly_t(8,2)( [0.0, 1.0, 0.0] )
    default_poly = poly_t(8,10)( coeffs=[1.0, 0.0, 0.0] )
  end subroutine demo

end module pdt_ctor_poly

