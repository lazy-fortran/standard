program test_data_implied_do_mixed
  implicit none
  integer :: arr1(5)
  integer :: arr2(3,4)
  real :: x, y, z

  ! Mix of simple variables and implied-DO
  data x, y, z / 1.0, 2.0, 3.0 /
  data (arr1(i), i=1,5) / 5*42 /
  data ((arr2(i,j), i=1,3), j=1,4) / 12*7 /

  print *, 'Mixed DATA test'
  print *, 'x, y, z:', x, y, z
  print *, 'arr1:', arr1
  print *, 'arr2:', arr2

end program test_data_implied_do_mixed
