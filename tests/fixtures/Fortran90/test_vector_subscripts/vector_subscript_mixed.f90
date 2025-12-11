program vector_subscript_mixed
  implicit none

  ! Mixed subscripts: scalar, triplet, and vector in same reference
  ! ISO/IEC 1539:1991 Section 6.2.2.1

  integer :: tensor(4, 5, 6)
  integer :: vec_indices(3)

  vec_indices = (/ 1, 3, 5 /)

  ! First dimension: vector subscript
  ! Second dimension: scalar subscript
  ! Third dimension: subscript triplet (start:end:stride)
  ! Access using mixed subscript types
  print *, tensor(vec_indices, 2, 1:6:2)

end program vector_subscript_mixed
