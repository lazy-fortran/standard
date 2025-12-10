program co_min_test
  implicit none
  integer :: min_val[*]

  min_val = 5
  call co_min(min_val, result_image=1)

end program co_min_test
