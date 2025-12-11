program test_data_implied_do_nested
  implicit none
  integer :: matrix(3,3)
  real :: grid(4,5)

  ! Nested implied-DO loops for 2D array initialization
  data ((matrix(i,j), i=1,3), j=1,3) / 1,2,3, 4,5,6, 7,8,9 /
  data ((grid(i,j), j=1,5), i=1,4) / 20*0.0 /

  print *, 'Nested implied-DO DATA test'
  print *, 'Matrix:'
  print *, matrix
  print *, 'Grid:'
  print *, grid

end program test_data_implied_do_nested
