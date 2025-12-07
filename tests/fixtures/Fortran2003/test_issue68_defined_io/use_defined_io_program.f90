program use_defined_io
  use defined_io_mod
  implicit none

  type(point_t) :: p

  p%x = 1.0
  p%y = 2.0

  ! DT edit descriptor inside format string (simplified example)
  write(*, '(DT"point"(10,2))') p
end program use_defined_io

