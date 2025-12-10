program co_broadcast_test
  implicit none
  integer :: data[*], source_img

  data = 42
  source_img = 1
  call co_broadcast(data, source_image=source_img)

end program co_broadcast_test
