program vector_subscript_rank_violation
  implicit none

  ! ISO/IEC 1539:1991 Section 6.2.2.1 (R621)
  ! Vector subscript MUST be rank-one array
  ! This fixture demonstrates INVALID rank-two vector subscript

  integer :: arr(10)
  integer :: indices_2d(3, 3)

  arr = (/ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 /)
  indices_2d = 1

  ! SEMANTIC VIOLATION: indices_2d is rank-two, not rank-one
  ! arr(indices_2d) = 99  ! Invalid: rank-two subscript

end program vector_subscript_rank_violation
