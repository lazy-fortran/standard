program vector_subscript_test
  implicit none

  ! ISO/IEC 1539:1991 Section 6.2.2.1 (R620-R621)
  ! Test basic vector subscript usage

  integer :: arr(10)
  integer :: indices(3)
  integer :: result(3)

  arr = (/ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 /)
  indices = (/ 2, 5, 8 /)

  ! Vector subscript: select arr(2), arr(5), arr(8)
  result = arr(indices)

  print *, result

end program vector_subscript_test
