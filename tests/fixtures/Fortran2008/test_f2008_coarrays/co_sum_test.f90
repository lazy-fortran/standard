program co_sum_test
  implicit none
  real :: sum_val[*]

  sum_val = 3.14
  call co_sum(sum_val)

end program co_sum_test
