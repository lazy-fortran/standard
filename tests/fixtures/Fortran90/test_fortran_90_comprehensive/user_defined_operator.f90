module defined_operator_test
  implicit none

  interface operator(.DOT.) module procedure dot_prod_custom
  end interface

  interface operator(.MAGNITUDE.) module procedure magnitude_custom
  end interface

contains

  real function dot_prod_custom(a, b)
    real, intent(in) :: a(3), b(3)
    dot_prod_custom = sum(a * b)
  end function dot_prod_custom

  real function magnitude_custom(v)
    real, intent(in) :: v(3)
    magnitude_custom = sqrt(sum(v * v))
  end function magnitude_custom

end module defined_operator_test
