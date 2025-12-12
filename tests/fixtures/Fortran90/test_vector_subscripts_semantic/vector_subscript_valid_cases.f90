program vector_subscript_valid_cases
  implicit none

  ! ISO/IEC 1539:1991 Section 6.2.2.1 (R621)
  ! Valid vector subscript test cases

  integer :: arr(20)
  integer :: indices(5)
  integer :: result(5)

  arr = (/ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20 /)

  ! Case 1: Vector subscript with simple array variable
  indices = (/ 2, 5, 8, 11, 14 /)
  result = arr(indices)

  ! Case 2: Vector subscript with array constructor
  result = arr( (/ 1, 3, 5, 7, 9 /) )

  ! Case 3: Vector subscript in multidimensional array
  ! (covered in separate test file)

  ! Case 4: Vector subscript result can be assigned to
  arr(indices) = (/ 100, 200, 300, 400, 500 /)

  print *, result

end program vector_subscript_valid_cases
