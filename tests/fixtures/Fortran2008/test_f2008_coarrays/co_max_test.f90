program co_max_test
  implicit none
  integer :: max_val[*]

  max_val = 10
  call co_max(max_val, result_image=1)

end program co_max_test
