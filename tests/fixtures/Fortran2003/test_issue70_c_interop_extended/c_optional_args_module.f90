module c_optional_args
  use iso_c_binding
  implicit none

contains

  subroutine c_log_message(msg, level) bind(c, name="c_log_message")
    character(len=*), intent(in) :: msg
    integer(c_int), value        :: level
  end subroutine c_log_message

end module c_optional_args

