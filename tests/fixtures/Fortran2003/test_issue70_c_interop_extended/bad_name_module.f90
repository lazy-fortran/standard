module bad_name
  implicit none
contains
  subroutine s() bind(c, name=foo)
  end subroutine s
end module bad_name

