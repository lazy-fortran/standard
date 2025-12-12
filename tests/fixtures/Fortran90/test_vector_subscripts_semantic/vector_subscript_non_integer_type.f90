program vector_subscript_non_integer_type
  implicit none

  ! ISO/IEC 1539:1991 Section 6.2.2.1 (R621)
  ! Vector subscript elements must be INTEGER type
  ! This fixture demonstrates INVALID real-valued vector subscript

  integer :: arr(10)
  real :: real_indices(3)

  arr = (/ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 /)
  real_indices = (/ 2.5, 5.7, 8.1 /)

  ! SEMANTIC VIOLATION: real_indices is REAL type, not INTEGER
  arr(real_indices) = 99

end program vector_subscript_non_integer_type
