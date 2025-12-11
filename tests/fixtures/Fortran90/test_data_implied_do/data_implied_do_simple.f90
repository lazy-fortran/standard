program test_data_implied_do_simple
  implicit none
  real :: a(10)
  integer :: b(5)
  logical :: flag(3)

  ! Simple implied-DO with single variable
  data (a(i), i=1,10) / 10*0.0 /
  data (b(i), i=1,5) / 5*1 /
  data (flag(i), i=1,3) / 3*.true. /

  print *, 'Simple implied-DO DATA test'
  print *, a
  print *, b
  print *, flag

end program test_data_implied_do_simple
