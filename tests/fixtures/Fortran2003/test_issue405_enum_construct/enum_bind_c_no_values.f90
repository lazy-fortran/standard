program enum_no_explicit_values
  implicit none

  enum, bind(c)
    enumerator :: first
    enumerator :: second
    enumerator :: third
  end enum

  integer :: val
  val = first

end program enum_no_explicit_values
