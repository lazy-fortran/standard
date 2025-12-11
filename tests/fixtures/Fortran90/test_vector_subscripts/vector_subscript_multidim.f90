program vector_subscript_multidim
  implicit none

  ! Vector subscripts with multidimensional arrays
  ! ISO/IEC 1539:1991 Section 6.2.2.1 (R620-R621)

  integer :: matrix(4, 5)
  integer :: row_indices(2)
  integer :: col_indices(3)

  row_indices = (/ 1, 3 /)
  col_indices = (/ 2, 3, 4 /)

  ! Vector subscripts on each dimension
  print *, matrix(row_indices, col_indices)

end program vector_subscript_multidim
