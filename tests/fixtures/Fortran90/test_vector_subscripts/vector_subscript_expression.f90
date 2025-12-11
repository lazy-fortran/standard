program vector_subscript_expression
  implicit none

  ! Vector subscripts using array expressions
  ! ISO/IEC 1539:1991 Section 6.2.2.1 (R621: int-expr as rank-one array)

  integer :: values(10)
  integer :: indices(4)
  integer :: evens(4)

  values = (/ 10, 20, 30, 40, 50, 60, 70, 80, 90, 100 /)

  ! Vector subscript using array constructor expression
  evens = values( (/ 2, 4, 6, 8 /) )

  print *, evens  ! Should print: 20 40 60 80

end program vector_subscript_expression
